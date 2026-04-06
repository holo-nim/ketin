import std/macros

type TypeSectionGen* = enum Statement, TypeSection, TypeDef

proc detectTypeSection*(arg: NimNode): TypeSectionGen =
  case arg.kind
  of nnkTypeDef:
    when (NimMajor, NimMinor) >= (2, 0): result = TypeSection
    else: result = TypeDef
  of nnkTypeSection: result = TypeSection # to be safe for any future changes
  else: result = Statement

proc wrap*(gen: TypeSectionGen, typeSection: NimNode, remaining: seq[NimNode]): NimNode =
  case gen
  of Statement:
    result = newStmtList()
    if not typeSection.isNil: result.add typeSection
    result.add remaining
  of TypeSection:
    result = if typeSection.isNil: newNimNode(nnkTypeSection) else: typeSection
    var i = 0
    while i < remaining.len:
      let st = remaining[i]
      case st.kind
      of nnkTypeDef:
        result.add st
        inc i
      of nnkTypeSection:
        for td in st: result.add td
        inc i
      else:
        var val = newNimNode(nnkStmtListType, typeSection)
        val.add st
        var j = i + 1
        while j < remaining.len and remaining[j].kind notin {nnkTypeDef, nnkTypeSection}:
          val.add remaining[j]
          inc j
        i = j
        val.add bindSym"void"
        result.add newTree(nnkTypeDef, genSym(nskType, "_"), newEmptyNode(), val)
    if result.len == 0:
      result.add newTree(nnkTypeDef, genSym(nskType, "_"), newEmptyNode(), bindSym"void")
  of TypeDef:
    if not typeSection.isNil and typeSection.len == 1:
      result = typeSection[0]
    else:
      var val = newNimNode(nnkStmtListType)
      if not typeSection.isNil: val.add typeSection
      val.add remaining
      val.add bindSym"void"
      result = newTree(nnkTypeDef, genSym(nskType, "_"), newEmptyNode(), val)
