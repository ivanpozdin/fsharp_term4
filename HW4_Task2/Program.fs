module Point_Free

let func x l = List.map (fun y -> y * x) l

let func'1 x = List.map (fun y -> y * x)
let func'2: (int -> int list -> int list) = List.map << (*)
