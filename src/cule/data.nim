import ./schema, glaze, std/[macrocache, macros]

proc getSchemaData*(id: SchemaId): CacheSeq =
  CacheSeq(getSchemaDataHandle(id))

type RawRow* = seq[RawNimNode]

proc addRawRow*(id: SchemaId, row: RawRow) =
  getSchemaData(id).add glaze(row)

macro defineRowAdd*(id: static SchemaId, name: untyped) =
  var name = name
  let isExported = name.kind in {nnkPrefix, nnkPostfix}
  if isExported: name = name[1]
  expectKind name, {nnkIdent, nnkAccQuoted, nnkSym, nnkOpenSymChoice, nnkClosedSymChoice}
  name = ident repr name
  let schema = getSchema(id)
  var params: seq[NimNode] = @[newEmptyNode()]
  var row = newNimNode(nnkBracket, name)
  for field in schema.fields:
    let typeNode = field.type.NimNode
    let defaultNode = field.default.NimNode
    var def = newNimNode(nnkIdentDefs, typeNode)
    def.add ident(field.name)
    if typeNode.isNil: def.add newEmptyNode()
    else: def.add typeNode
    if defaultNode.isNil: def.add newEmptyNode()
    else: def.add defaultNode
    params.add def
    row.add newCall(bindSym"RawNimNode", ident(field.name)) # XXX needs to be different for typedescs and statics
  result = newProc(
    procType = nnkMacroDef,
    name = name,
    params = params,
    body = newCall(bindSym"addRawRow", glaze id, newCall(bindSym"@", row)))

iterator eachRawRow*(id: SchemaId): RawRow =
  for rowNode in getSchemaData(id):
    yield deglaze(rowNode, RawRow)
