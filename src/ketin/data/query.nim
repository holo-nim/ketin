import ./schema, glaze, std/[macrocache, macros]

proc getSchemaData*(id: SchemaId): CacheSeq =
  CacheSeq(getSchemaDataHandle(id))

type
  RawRow* = seq[RawNimNode]
  EmptyColumn* = object
    ## representation of `nil` node passed to users

proc addRawRow*(id: SchemaId, row: RawRow) =
  if isFrozen(id):
    raise newException(FrozenError, "schema " & $id & " is frozen, no rows can be added")
  getSchemaData(id).add glaze(row)

proc defineRowAdd*(id: SchemaId, rawName: NimNode, initialParams: seq[NimNode] = @[newEmptyNode()], genericParams: NimNode = nil): NimNode =
  var rawName = rawName
  let isExported = rawName.kind in {nnkPrefix, nnkPostfix}
  if isExported: rawName = rawName[1]
  expectKind rawName, {nnkIdent, nnkAccQuoted, nnkSym, nnkOpenSymChoice, nnkClosedSymChoice}
  #name = ident repr name
  let schema = getSchema(id)
  var params: seq[NimNode] = initialParams
  var row = newNimNode(nnkBracket, rawName)
  for field in schema.fields:
    var typeNode = field.type.constraint.NimNode
    case field.type.kind
    of ExprAtom:
      if typeNode.isNil or typeNode.kind == nnkNilLit:
        typeNode = bindSym"untyped"
    of TypeAtom:
      if typeNode.isNil or typeNode.kind == nnkNilLit:
        typeNode = bindSym"typedesc"
      else:
        typeNode = newTree(nnkBracketExpr, bindSym"typedesc", typeNode)
    of StaticAtom:
      if typeNode.isNil or typeNode.kind == nnkNilLit:
        typeNode = bindSym"static"
      else:
        typeNode = newTree(nnkBracketExpr, bindSym"static", typeNode)
    let defaultNode = field.default.NimNode
    var def = newNimNode(nnkIdentDefs, typeNode)
    def.add ident(field.name)
    def.add typeNode
    if defaultNode.isNil: def.add newEmptyNode()
    else: def.add defaultNode
    params.add def
    var serializeNode = ident(field.name)
    case field.type.kind
    of ExprAtom: discard
    of TypeAtom:
      # maybe implement glaze for typedesc
      # might need wrapping in `type()`
      serializeNode = newCall(bindSym"getTypeInst", serializeNode)
    of StaticAtom:
      serializeNode = newCall(bindSym("glaze", brForceOpen), serializeNode)
    row.add newCall(bindSym"RawNimNode", serializeNode)
  result = newProc(
    procType = nnkMacroDef,
    name = rawName,
    params = params,
    body = newCall(bindSym"addRawRow", glaze id, newCall(bindSym"@", row)))
  if not genericParams.isNil:
    result[2] = genericParams

proc currentCount*(id: SchemaId): int =
  # no freeze requirement?
  len(getSchemaData(id))

iterator eachRawRow*(id: SchemaId): RawRow =
  if readRequiresFreeze(id) and not isFrozen(id):
    raise newException(NotFrozenError, "schema " & $id & " requires being frozen to read but is not frozen")
  for rowNode in getSchemaData(id):
    yield deglaze(rowNode, RawRow)

template isColumnEmpty*(value: NimNode): bool =
  # `nil` nodes are preserved in macrocache, so they are used for the empty state
  isNil(value)

template columnExists*(value: NimNode): bool =
  not isColumnEmpty(value)

# single column operations:

proc doPick*(id: SchemaId, column: int, ignoreRest = true): tuple[found: int, value: NimNode] =
  result.found = 0
  for row in eachRawRow(id):
    let node = row[column].NimNode
    if columnExists(node):
      inc result.found
      if result.found == 1:
        result.value = node
        if ignoreRest: return
      else:
        return

proc doChoice*(id: SchemaId, column: int): NimNode =
  result = newNimNode(nnkClosedSymChoice)
  for row in eachRawRow(id):
    let node = row[column].NimNode
    if columnExists(node):
      case node.kind
      of nnkSym: result.add(node)
      of nnkClosedSymChoice, nnkOpenSymChoice:
        for s in node: result.add(s)
      else:
        error "non-symbol node kind " & $node.kind & " for choice of " & $column & " in " & $id, node

proc doCount*(id: SchemaId, column: int): int =
  result = 0
  for row in eachRawRow(id):
    let node = row[column].NimNode
    if columnExists(node):
      inc result

proc doCollect*(id: SchemaId, column: int, result: NimNode) =
  for row in eachRawRow(id):
    let node = row[column].NimNode
    if columnExists(node):
      result.add node

proc copyApplyPattern*(root: NimNode): NimNode =
  if root.kind in nnkCallKinds + {nnkBracket, nnkPar, nnkTupleConstr, nnkCurly}:
    result = newNimNode(root.kind, root)
    for a in root: result.add root # not copied?
  else:
    result = newCall(root)

proc makeApplicable*(root: NimNode): NimNode =
  if root.kind in nnkCallKinds + {nnkBracket, nnkPar, nnkTupleConstr, nnkCurly}:
    result = root
  else:
    result = newCall(root)

proc applyRow*(call: NimNode, row: RawRow) =
  # works for calls, collection literals, etc
  for col in row:
    let node = col.NimNode
    if columnExists(node):
      call.add node
    else:
      call.add newTree(nnkObjConstr, bindSym"EmptyColumn")

proc doUnravel*(id: SchemaId, callPattern: NimNode): NimNode =
  ## generates statement list that calls `callPattern` with each row
  result = newNimNode(nnkStmtList, callPattern)
  for row in eachRawRow(id):
    var call = copyApplyPattern(callPattern)
    applyRow(call, row)
    result.add call

proc doDispatch*(id: SchemaId, column: int, value: NimNode, callPattern: NimNode, elseBody: NimNode): NimNode =
  ## generates a `case` statement for `value` based on the values of `column`,
  ## calls `callPattern` with found row
  ## 
  ## if `elseBody` is not `nil`, generates `else`
  result = newNimNode(nnkCaseStmt, value)
  result.add value
  for row in eachRawRow(id):
    let node = row[column].NimNode
    if columnExists(node):
      var branch = newNimNode(nnkOfBranch, node)
      branch.add node
      var call = copyApplyPattern(callPattern)
      applyRow(call, row)
      branch.add call
      result.add branch
  if not elseBody.isNil:
    result.add newTree(nnkElse, elseBody)

proc doFind*[T](id: SchemaId, column: int, value: T, call: NimNode, elseBody: NimNode): NimNode =
  ## checks for equality to `value` in the static values of `column`,
  ## calls `call` with found row, otherwise returns `elseBody`
  for row in eachRawRow(id):
    let node = row[column].NimNode
    if columnExists(node):
      if value == deglaze(node, T):
        result = makeApplicable(call)
        applyRow(result, row)
        return result
  result = elseBody
