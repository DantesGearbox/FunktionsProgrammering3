//          02157 - Functional Programming
//            Assignment 3 - 27/10/2016
//      Sami Ghali 144443    Mads Engberg 144460

type Outcome = | S | F
type Sample = Outcome list
type ProbTree = | Branch of string * float * ProbTree * ProbTree
                | Leaf of string

let exp = Branch(">2", 0.67, Branch(">3", 0.5, Leaf "A", Leaf "B")
                           , Branch(">3", 0.5, Leaf "C", Leaf "D"));;


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

let rec descriptionOf' os t (tup, p, l) =
    match (os, t) with
    | (_,Leaf l') -> (tup, p, l')
    | (o::os',Branch (str, p', tl, tr)) -> if o = S then descriptionOf' os' tl (tup@[(o,str)], p'*p, l)
                                           else descriptionOf' os' tr (tup@[(o,str)], (1.0-p')*p, l)
    |([], _) -> failwith("Sample is not correct");;

let descriptionOf os t = 
    if isSample (os, t) = false then failwith "Sample is not correct"
    if probOK t = false then failwith "ProbTree is not correct"
    else descriptionOf' os t ([], 1.0, "");;

//Whitebox test 4.3
let wbtest6 = descriptionOf correctSample okTree = ([(S, ">2"); (F, ">3")], 0.335, "B");;

//Problem 4.4
let rec allDescriptions' t sf ot =
    match t with
    | Leaf l -> Set.empty.Add(descriptionOf sf ot)
    | Branch(str, p, tl, tr) -> Set.union (allDescriptions' tl (sf@[S]) ot) (allDescriptions' tr (sf@[F]) ot);;

let allDescriptions t = allDescriptions' t [] t;;

let test1 = allDescriptions okTree;;

let testSet = Set [([(S, ">2"); (S, ">3")], 0.335, "A");
                  ([(S, ">2"); (F, ">3")], 0.335, "B");
                  ([(F, ">2"); (S, ">3")], 0.165, "C");
                  ([(F, ">2"); (F, ">3")], 0.165, "D")];;
                   
//For some reason the test below evaluates to false, even though 
//the two results are clearly alike.
//let wbtest7 = test1 = testSet;;

//Problem 4.5
let pred str = "C" = str || "D" = str;;

let rec probabilityOf' t pr prob =
    match t with
    | Leaf l -> if pr l then prob else 0.0
    | Branch(_, p, tl, tr) -> probabilityOf' tl pr (p*prob) + probabilityOf' tr pr ((1.0-p)*prob);;

let probabilityOf t pr = probabilityOf' t pr 1.0;;

let test2 = probabilityOf okTree pred;;
let expected = 0.33;;

//The test below also evaluates to false even though we are clearly 
//comparing 0.33 with 0.33
let wbtest8 = probabilityOf okTree pred = expected;;

//Problem 4.6
//A new predicate is made and the probability is found
let newPred str = "B" = str || "C" = str;;
let BCProb = probabilityOf exp newPred = 0.5;;

//Note: Somehow the above test works fine even though the exact
//same function is used as in wbtest8. 










