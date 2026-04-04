import glaze, std/macrocache

when false:
  type
    CuleTypeKind* = enum
      CuleExpr
      CuleTypedesc
      CuleStatic
      CuleTyped
    CuleType* = object
      case kind*: CuleTypeKind
      of CuleExpr:
        exprTyped*: bool
      of CuleTypedesc, CuleStatic, CuleTyped:
        constraint*: RawNimNode

type
  CuleField* = object
    column*: int
    name*: string
    `type`*: RawNimNode # can be untyped, typed, static, typedesc, or other typed node
    default*: RawNimNode
  CuleSchema* = object
    fields*: seq[CuleField]

proc toNode*(schema: CuleSchema): NimNode =
  glaze(schema)

proc toSchema*(node: NimNode): CuleSchema =
  deglaze(node, result)

type SchemaId* = string

const schemas* = CacheTable"cule.schemas"

proc addSchema*(id: SchemaId, schema: CuleSchema) =
  schemas[id] = toNode(schema)

proc getSchema*(id: SchemaId): CuleSchema =
  toSchema(schemas[id])

proc getSchemaDataHandle*(id: SchemaId): string =
  "cule.data." & id
