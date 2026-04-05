import std/[macros, macrocache]

const typeIdCounter = CacheCounter"keten.typeids"

type TypeId* = distinct int
  # currently uses a counter
  # if this causes problems, can use something like `signatureHash` instead

macro newId(_: typed): TypeId =
  # typed argument is to prevent early macro instantiation in generic,
  # not needed with https://github.com/nim-lang/Nim/pull/22517
  inc typeIdCounter
  result = newCall(bindSym"TypeId", newLit(typeIdCounter.value))

proc getTypeId*(T: type): TypeId =
  const id = newId(0)
    # only generated when proc is compiled, i.e. instantiated according to `T`
  id
