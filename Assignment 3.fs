type Outcome = | S | F
type Sample = Outcome list
type ProbTree = | Branch of string * float * ProbTree * ProbTree
                | Leaf of string

let exp = Branch(">2", 0.67, Branch(">3", 0.5, Leaf "A", Leaf "B")
                           , Branch(">3", 0.5, Leaf "C", Leaf "D" ))

