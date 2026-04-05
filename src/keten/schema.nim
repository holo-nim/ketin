import glaze, std/macrocache

type
  AtomKind* = enum
    ExprAtom
    TypeAtom
    StaticAtom
  AtomType* = object
    case kind*: AtomKind
    of ExprAtom, TypeAtom, StaticAtom:
      constraint*: RawNimNode
  KetenField* = object
    # or "strand"
    column*: int
    name*: string
    `type`*: AtomType # can be untyped, typed, static, typedesc, or other typed node
    default*: RawNimNode
  KetenSchema* = object
    # or "fabric"
    fields*: seq[KetenField]

proc getColumn*(schema: KetenSchema, name: string): int =
  result = -1
  for i, field in schema.fields:
    if field.name == name:
      return i

proc toNode*(schema: KetenSchema): NimNode =
  glaze(schema)

proc toSchema*(node: NimNode): KetenSchema =
  deglaze(node, result)

type SchemaId* = string

const schemas* = CacheTable"keten.schemas"

proc addSchema*(id: SchemaId, schema: KetenSchema) =
  schemas[id] = toNode(schema)

proc getSchema*(id: SchemaId): KetenSchema =
  toSchema(schemas[id])

type FrozenError* = object of CatchableError

const freezes* = CacheTable"keten.freezes"

proc freeze*(id: SchemaId) {.compileTime.} =
  freezes[id] = glaze true

proc isFrozen*(id: SchemaId): bool {.compileTime.} =
  id in freezes and deglaze(freezes[id], bool)

proc getSchemaDataHandle*(id: SchemaId): string =
  "keten.data." & id

# maybe indexes?
