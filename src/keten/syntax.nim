import glaze, ./[schema, data], std/[macros, macrocache], cosm/caseutils

type TypeSectionGen = enum Statement, TypeSection, TypeDef

proc wrap(gen: TypeSectionGen, typeSection: NimNode, remaining: seq[NimNode]): NimNode =
  case gen
  of Statement:
    result = newStmtList()
    if not typeSection.isNil: result.add typeSection
    result.add remaining
  of TypeSection:
    result = if typeSection.isNil: newNimNode(nnkTypeSection) else: typeSection
    var val = newNimNode(nnkStmtListType, typeSection)
    val.add remaining
    val.add bindSym"void"
    result.add newTree(nnkTypeDef, genSym(nskType, "_"), newEmptyNode(), val)
  of TypeDef:
    if not typeSection.isNil and typeSection.len == 1:
      result = typeSection[0]
    else:
      var val = newNimNode(nnkStmtListType)
      if not typeSection.isNil: val.add typeSection
      val.add remaining
      val.add bindSym"void"
      result = newTree(nnkTypeDef, genSym(nskType, "_"), newEmptyNode(), val)

type
  FabricKind* = enum
    Warm, Cold
  FabricOptions* = object
    requiresFreeze*: bool
    id*: string

proc processFabricType(node: NimNode): KetenAtomType =
  if node.kind in nnkCallKinds + {nnkBracket} and node.len == 2:
    if node[0].eqIdent"typedesc" or node[0].eqIdent"type":
      return KetenAtomType(kind: TypeAtom, constraint: RawNimNode node[1])
    elif node[0].eqIdent"static":
      return KetenAtomType(kind: StaticAtom, constraint: RawNimNode node[1])
  elif node.kind in {nnkIdent, nnkAccQuoted, nnkSym, nnkOpenSymChoice, nnkClosedSymChoice}:
    if node.eqIdent"typedesc" or node.eqIdent"type":
      return KetenAtomType(kind: TypeAtom, constraint: RawNimNode nil)
    elif node.eqIdent"static":
      return KetenAtomType(kind: StaticAtom, constraint: RawNimNode nil)
  result = KetenAtomType(kind: ExprAtom, constraint: RawNimNode node)

proc iterFabricFields(fields: var seq[KetenField], node: NimNode) =
  case node.kind
  of nnkRecList, nnkTupleTy:
    for n in node:
      iterFabricFields(fields, n)
  of nnkRecWhen:
    error("when not supported in fabric", node)
  of nnkRecCase:
    error("case not allowed in fabric", node)
  of nnkIdentDefs:
    let typPos = node.len - 2
    let typ = processFabricType(node[typPos])
    for i in 0 ..< typPos:
      var nameNode = node[i]
      if nameNode.kind == nnkPragmaExpr: nameNode = nameNode[0]
      if nameNode.kind == nnkPostfix: nameNode = nameNode[1]
      let name = $nameNode
      let col = fields.len
      fields.add KetenField(column: col, name: name, type: typ, default: RawNimNode(copy node[typPos + 1]))
  of nnkDiscardStmt, nnkNilLit, nnkEmpty: discard
  else:
    error("invalid type ast for fabric: " & $node.kind, node)

proc buildFabric(options: FabricOptions, body: NimNode): tuple[typeSection: NimNode, remaining: seq[NimNode]] =
  if body.kind in {nnkStmtList, nnkTypeSection}:
    for b in body:
      let (types, remaining) = buildFabric(options, b)
      if result.typeSection.isNil:
        result.typeSection = types
      elif result.remaining.len == 0:
        for td in types: result.typeSection.add td
      else:
        result.remaining.add types
      result.remaining.add remaining
    return result
  expectKind body, nnkTypeDef
  var nameNode = body[0]
  if nameNode.kind == nnkPragmaExpr: nameNode = nameNode[0]
  let isPostfix = nameNode.kind == nnkPostfix
  if isPostfix: nameNode = nameNode[1]
  let name = $nameNode
  var schema = KetenSchema(readRequiresFreeze: options.requiresFreeze)
  if body[2].kind != nnkObjectTy:
    error "expected object type for fabric", body[2]
  iterFabricFields(schema.fields, body[2][^1])
  var rawId = options.id
  if rawId.len == 0:
    rawId = $schemas.len & "-" & toKebabCase(name)
  let id = SchemaId(rawId)
  addSchema id, schema
  var newType = newNimNode(nnkTypeDef, body)
  newType.add body[0]
  newType.add body[1]
  newType.add newTree(nnkObjectTy, newEmptyNode(), newEmptyNode(), newEmptyNode())
  result.typeSection = newTree(nnkTypeSection, newType)
  result.remaining.add newProc(
    procType = nnkTemplateDef,
    name =
      if isPostfix: newTree(nnkPostfix, ident"*", ident"getFabricSchemaId")
      else: ident"getFabricSchemaId",
    params = [bindSym"SchemaId",
      newTree(nnkIdentDefs, ident"T", newTree(nnkBracketExpr, ident"typedesc", ident name), newEmptyNode())],
    body = newLit id
  )
  var stitchName = ident"stitch"
  if isPostfix: stitchName = newTree(nnkPostfix, ident"*", stitchName)
  var stitchParams = @[newEmptyNode()]
  stitchParams.add newTree(nnkIdentDefs, ident"T", newTree(nnkBracketExpr, ident"typedesc", ident name), newEmptyNode())
  result.remaining.add defineRowAdd(id, stitchName, stitchParams)

proc fabricImpl(options: FabricOptions, body: NimNode): NimNode =
  let gen =
    case body.kind
    of nnkTypeDef:
      when (NimMajor, NimMinor) >= (2, 0): TypeSection
      else: TypeDef
    of nnkTypeSection: TypeSection
    else: Statement
  let (typeSection, remaining) = buildFabric(options, body)
  result = wrap(gen, typeSection, remaining)

macro fabric*(option: static tuple[], body: untyped): untyped =
  fabricImpl(FabricOptions(), body)

macro fabric*(option: static FabricKind, body: untyped): untyped =
  fabricImpl(FabricOptions(requiresFreeze: option == Cold), body)

macro fabric*(option: static string, body: untyped): untyped =
  fabricImpl(FabricOptions(id: option), body)

macro fabric*(option: static FabricOptions, body: untyped): untyped =
  fabricImpl(option, body)

macro dispatchImpl(id: static SchemaId, column: untyped, value: typed, toApply: untyped, elses: varargs[untyped]): untyped =
  let schema = getSchema(id)
  let realColumn =
    if column.kind in {nnkIntLit..nnkUint64Lit}: column.intVal.int
    else: getColumn(schema, $column)
  var callPattern = toApply
  result = newStmtList()
  if toApply.kind in {nnkLambda, nnkDo}:
    let (kind, symkind, kindname) =
      if toApply.kind == nnkLambda: (nnkProcDef, nskProc, "Proc")
      else: (nnkTemplateDef, nskTemplate, "Templ")
    var routine = newNimNode(kind, toApply)
    for child in toApply: routine.add child
    let routineName = genSym(symkind, "dispatch" & kindname)
    routine[0] = routineName
    callPattern = routineName
    result.add routine
  var realElse: NimNode = nil
  if elses.len > 0:
    if elses.len != 1: error "only 1 else supported", elses
    if elses[0].kind != nnkElse: error "expected else", elses[0]
    realElse = elses[0][0]
  result.add dispatch(id, realColumn, value, callPattern, realElse)

macro dispatch*(T: typedesc, column: untyped, value: typed, toApply: untyped, elses: varargs[untyped]): untyped =
  result = newCall(bindSym"dispatchImpl", newCall(ident"getFabricSchemaId", T), column, value, toApply)
  for e in elses: result.add e
