$version: "2.0"

namespace example

structure Foo {
    a: Integer
    b: String
}

union Bar {
    a: Integer
    b: String
    c: Unit
}

structure RecA {
    b: RecB
}

structure RecB {
    @required
    a: RecA
}
