# Package

version       = "0.1.0"
author        = "metagn"
description   = "description"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 1.6.0"
requires "https://github.com/holo-nim/glaze"
requires "https://github.com/holo-nim/cosm" # already required by glaze but also used for caseutils

task docs, "build docs for all modules":
  exec "nim r tasks/build_docs.nim"

task tests, "run tests for multiple backends and defines":
  exec "nim r tasks/run_tests.nim"
