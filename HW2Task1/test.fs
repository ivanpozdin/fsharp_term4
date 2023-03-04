module HW2Task1.test

open HW2Task1.functions
open FsCheck

let doTest (ls: list<int>) =
    (evenCount1 ls) = (evenCount2 ls) && (evenCount1 ls) = (evenCount3 ls)

Check.Quick doTest
