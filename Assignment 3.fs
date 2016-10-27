//          02157 - Functional Programming
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
//  isSample: Sample * ProbTree -> bool

// Problem 4.2
let rec isSample (os, t) = 
    match (os, t) with
    | ([], Leaf _) -> true
    | (_, Leaf _) -> false
    | ([], Branch _) -> false
    | (o::os', Branch(_, _, tl, tr)) -> if o = S then isSample (os', tl) else  isSample (os', tr);;

//Whitebox tests 4.2
let correctSample = [S; F]
let badSample1 = [S; F; F]
let badSample2 = [S]

let wbtest3 = isSample (correctSample, okTree) = true;;
let wbtest4 = isSample (badSample1, okTree) = false;;
let wbtest5 = isSample (badSample2, okTree) = false;;

//Problem 4.3
type Description = (Outcome * string) list * float * string

let rec findList os t = 
    match (os, t) with
    | ([], _) -> []
    | (_, Leaf l) -> []
    | (o::os',Branch(str, _, tl, tr)) -> if o = S then (o, str)::findList os' tl else (o, str)::findList os' tr;;

let rec findProb os t = 
    match (os, t) with
    | ([], _) -> 1.0
    | (_, Leaf l) -> 1.0
    | (o::os',Branch(_, p, tl, tr)) -> if o = S then p * findProb os' tl else (1.0-p) * findProb os' tr;;
 
let rec findLeaf os t =
    match (os, t) with
    | (_, Leaf l) -> l
    | (o::os',Branch(_, _, tl, tr)) ->if o = S then findLeaf os' tl else findLeaf os' tr
    | ([], _) -> "";;

let descriptionOf os t = 
    if isSample (os, t) = false then failwith "Sample is not correct"
    if probOK t = false then failwith "ProbTree is not correct"
    else (findList os t, findProb os t, findLeaf os t);; 
    //It would appear that there's a solution that runs through the tree
    //only once, but we weren't able to make the outcome work in a tuple 
    //which satisfies the example shown in the task description.

//Whitebox test 4.3
let wbtest6 = descriptionOf correctSample okTree = ([(S, ">2"); (F, ">3")], 0.335, "B");;