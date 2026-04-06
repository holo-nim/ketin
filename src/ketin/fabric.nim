## abstraction over schemas by attaching to a type

import data/[schema, query], utils/typemacros, glaze, std/[macros, macrocache], cosm/caseutils

type
  FabricKind* = enum
    Warm, Cold
  FabricOptions* = object
    requiresFreeze*: bool
    id*: string
  SomeFabric* = concept
    proc getFabricSchemaId(self: typedesc[Self]): SchemaId

proc processFabricType(node: NimNode): KetinAtomType =
  if node.kind in nnkCallKinds + {nnkBracket} and node.len == 2:
    if node[0].eqIdent"typedesc" or node[0].eqIdent"type":
      return KetinAtomType(kind: TypeAtom, constraint: RawNimNode node[1])
    elif node[0].eqIdent"static":
      return KetinAtomType(kind: StaticAtom, constraint: RawNimNode node[1])
  elif node.kind in {nnkIdent, nnkAccQuoted, nnkSym, nnkOpenSymChoice, nnkClosedSymChoice}:
    if node.eqIdent"typedesc" or node.eqIdent"type":
      return KetinAtomType(kind: TypeAtom, constraint: RawNimNode nil)
    elif node.eqIdent"static":
      return KetinAtomType(kind: StaticAtom, constraint: RawNimNode nil)
  result = KetinAtomType(kind: ExprAtom, constraint: RawNimNode node)

proc iterFabricFields(fields: var seq[KetinField], node: NimNode) =
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
      fields.add KetinField(column: col, name: name, type: typ, default: RawNimNode(copy node[typPos + 1]))
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
  var schema = KetinSchema(readRequiresFreeze: options.requiresFreeze)
  if body[2].kind != nnkObjectTy:
    error "expected object type for fabric", body[2]
  iterFabricFields(schema.fields, body[2][^1])
  var rawId = options.id
  if rawId.len == 0:
    rawId = $schemas.len & "-" & toKebabCase(name, uncapitalized = true)
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
  let gen = detectTypeSection(body)
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

type FabricColumn* = object
  schemaId*: SchemaId
  num*: int

macro columnImpl(id: static SchemaId, column: untyped): FabricColumn =
  let realColumn =
    if column.kind in {nnkIntLit..nnkUint64Lit}: column.intVal.int
    else: getColumn(id, $column)
  let columnCount = columnCount(id)
  if realColumn < 0 or realColumn >= columnCount:
    error "column " & repr(column) & " not in schema with " & $columnCount & " fields", column
  result = glaze FabricColumn(schemaId: id, num: realColumn)

template column*[T: SomeFabric](_: typedesc[T], column: untyped): FabricColumn =
  columnImpl(getFabricSchemaId(T), column)

proc promoteLambda(node: NimNode, baseName: string, stmts: NimNode, used = false): NimNode =
  let (kind, symkind, kindname) =
    if node.kind == nnkLambda: (nnkProcDef, nskProc, "Proc")
    else: (nnkTemplateDef, nskTemplate, "Templ")
  var routine = newNimNode(kind, node)
  for child in node: routine.add child
  if used:
    let prag = ident"used"
    if routine[4].kind == nnkEmpty:
      routine[4] = newTree(nnkPragma, prag)
    else:
      assert routine[4].kind == nnkPragma
      routine[4].add prag
  result = genSym(symkind, baseName & kindname)
  routine[0] = result
  stmts.add routine

macro unravelImpl(id: static SchemaId, toApply: untyped): untyped =
  var callPattern = toApply
  result = newStmtList()
  if toApply.kind in {nnkLambda, nnkDo}:
    callPattern = promoteLambda(toApply, "unravel", result)
  result.add doUnravel(id, callPattern)

template unravel*[T: SomeFabric](_: typedesc[T], toApply: untyped): untyped =
  unravelImpl(getFabricSchemaId(T), toApply)

proc unwrapElses(elses: NimNode): NimNode =
  ## expects nnkArgList
  if elses.len > 0:
    case elses[0].kind
    of nnkElifBranch, nnkElifExpr:
      var ifStmt = newNimNode(nnkIfStmt, elses[0])
      for i in 0 ..< elses.len:
        case elses[i].kind
        of nnkElifBranch, nnkElifExpr:
          ifStmt.add elses[i]
        of nnkElse, nnkElseExpr, nnkStmtList, nnkStmtListExpr:
          ifStmt.add elses[i]
          if i + 1 < elses.len:
            error("unexpected extra else branch", elses[i + 1])
          break
        else:
          error("expected else branch", elses[i])
      result = ifStmt
    of nnkElse, nnkElseExpr:
      result = elses[0][0]
      if elses.len > 1:
        error("unexpected extra else branch", elses[1])
    of nnkStmtList, nnkStmtListExpr:
      result = elses[0]
      if elses.len > 1:
        error("unexpected extra else branch", elses[1])
    else:
      error("expected else branch", elses[0])
  else:
    result = nil

macro dispatch*(column: static FabricColumn, value: typed, toApply: untyped, elses: varargs[untyped]): untyped =
  var callPattern = toApply
  result = newStmtList()
  if toApply.kind in {nnkLambda, nnkDo}:
    callPattern = promoteLambda(toApply, "dispatch", result)
  let realElse = unwrapElses(elses)
  result.add doDispatch(column.schemaId, column.num, value, callPattern, realElse)

macro find*(column: static FabricColumn, value: typed, toApply: untyped, elses: varargs[untyped]): untyped =
  var callPattern = toApply
  result = newStmtList()
  if toApply.kind in {nnkLambda, nnkDo}:
    callPattern = promoteLambda(toApply, "find", result, used = true) # gives false unused warning for some reason
  let realElse = unwrapElses(elses)
  let implName = genSym(nskMacro, "findImpl")
  var implParams = @[ident"untyped",
    newIdentDefs(ident"toApplyImpl", ident"untyped")]
  if not realElse.isNil:
    implParams.add newIdentDefs(ident"elseImpl", ident"untyped")
  var implOp = newCall(bindSym"doFind",
    newLit column.schemaId,
    newLit column.num,
    value,
    ident"toApplyImpl",
    if realElse.isNil: newNilLit() else: ident"elseImpl")
  let impl = newProc(
    procType = nnkMacroDef,
    name = implName,
    params = implParams,
    body = newAssignment(ident"result", implOp)
  )
  result.add impl
  var implCall = newCall(implName, callPattern)
  if not realElse.isNil: implCall.add realElse
  result.add implCall

# XXX implement:
# * dispatch `case` in object type
# * pragma to easily stitch types i.e. `type Foo {.stitch: (a, b).} = object ...` calls `stitch(a, b, Foo)`
#   maybe define per scheme or allow user to define it easily to also support optional arguments
