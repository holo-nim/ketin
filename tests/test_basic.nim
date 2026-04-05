import keten

type Foo {.fabric: ().} = object
  id: static int
  name: static string
  typ: typedesc

stitch Foo, 0, "abc", int
Foo.stitch 1, "def", float
Foo.stitch 2, "ghi", string

import std/strutils

proc take(s: string, T: type int): T = parseInt(s)
proc take(s: string, T: type float): T = parseFloat(s)
proc take(s: string, T: type string): T = s

proc print(x: int): string = "got int: " & $x
proc print(x: float): string = "got float: " & $x
proc print(x: string): string = "got string: " & $x

proc process(a: int, b: string): string =
  Foo.dispatch(id, a) do (id, name, typ):
    let val = take(b, typ)
    result = print(val) & " for " & name
  else: result = "invalid id: " & $a

doAssert process(0, "123") == "got int: 123 for abc"
doAssert process(0, "4000") == "got int: 4000 for abc"
doAssert process(0, "-5") == "got int: -5 for abc"
doAssert process(1, "1.23") == "got float: 1.23 for def"
doAssert process(1, "NaN") == "got float: nan for def"
doAssert process(2, "xyz") == "got string: xyz for ghi"
doAssert process(3, "...") == "invalid id: 3"
doAssert process(5, "") == "invalid id: 5" 
doAssert process(-1, "...") == "invalid id: -1"
