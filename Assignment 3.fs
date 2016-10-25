type Outcome = | S | F
type Sample = Outcome list
type ProbTree = | Branch of string * float * ProbTree * ProbTree
                | Leaf of string

let exp = Branch(">2", 0.67, Branch(">3", 0.5, Leaf "A", Leaf "B")
                           , Branch(">3", 0.5, Leaf "C", Leaf "D" ))

//Problem 4.1
let rec probOK pt = 
    match pt with
    | Leaf s -> true
    | Branch(_, prob, pt1, pt2) -> (0.0 < prob && prob < 1.0) && probOK pt1 && probOK pt2;;




