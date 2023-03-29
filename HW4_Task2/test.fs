open Point_Free

open FsCheck

let doTest (ls: list<int>, x: int) =
    func x ls = func'2 x ls && func'1 x ls = func'2 x ls

Check.Quick doTest
