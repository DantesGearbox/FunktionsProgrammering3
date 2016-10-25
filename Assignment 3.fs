//          02157 - Functional Programming
//            Assignment 3 - 27/10/2016
//      Sami Ghali 144443    Mads Engberg 144460


type Outcome = | S | F
type Sample = Outcome list
type ProbTree = | Branch of string * float * ProbTree * ProbTree
                | Leaf of string

let exp = Branch(">2", 0.67, Branch(">3", 0.5, Leaf "A", Leaf "B")
                           , Branch(">3", 0.5, Leaf "C", Leaf "D" ))

