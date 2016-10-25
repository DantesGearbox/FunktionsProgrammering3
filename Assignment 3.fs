﻿//          02157 - Functional Programming
//            Assignment 3 - 27/10/2016
//      Sami Ghali 144443    Mads Engberg 144460

type Outcome = | S | F
type Sample = Outcome list
type ProbTree = | Branch of string * float * ProbTree * ProbTree
                | Leaf of string

//Problem 4.1
let rec probOK pt = 
    match pt with
    | Leaf l -> true
    | Branch(_, prob, pt1, pt2) -> (0.0 < prob && prob < 1.0) && probOK pt1 && probOK pt2;;

//Whitebox tests 4.1
let okTree = Branch(">2", 0.67, Branch(">3", 0.5, Leaf "A", Leaf "B"), Branch(">3", 0.5, Leaf "C", Leaf "D" ))
let badTree = Branch(">2", 1.5, Branch(">3", 3.3, Leaf "A", Leaf "B"), Branch(">3", -0.5, Leaf "C", Leaf "D" ))

let wbtest1 = probOK okTree = true;;
let wbtest2 = probOK badTree = false;;


//The type of isSample is:
//  isSample: (Sample, probTree) -> bool

// Problem 4.2
let rec isSample (os, t) = 
    match (os, t) with
    | ([], Leaf _) -> true
    | (_, Leaf _) -> false
    | ([], Branch _) -> false
    | (o::os', Branch(_, _, tl, tr)) -> if o = S then isSample (os', tr) else  isSample (os', tl);;

//Whitebox tests 4.2
let correctSample = [S; F]
let badSample1 = [S; F; F]
let badSample2 = [S]


let wbtest3 = isSample (correctSample, okTree) = true;;
let wbtest4 = isSample (badSample1, okTree) = false;;
let wbtest5 = isSample (badSample2, okTree) = false;;
