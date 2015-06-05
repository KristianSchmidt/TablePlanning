#r @"..\packages\FSharp.Data.2.1.1\lib\net40\FSharp.Data.dll"
#r @"..\packages\Google.OrTools.1.0.3854\lib\net40\Google.OrTools.dll"

#load "Domain.fs"
#load "Data.fs"

open Domain
open Data
open Google.OrTools
open Google.OrTools.LinearSolver

let solver = Solver.CreateSolver("IntegerProgramming", "GLOP_LINEAR_PROGRAMMING")

type LinearExpr with
    static member sum (exprs : Variable list) =
        List.fold (+) (new LinearExpr()) exprs
    
let numTables = 10
let tableSeats = 8
let numGuests = Data.guests |> List.length

let guestIdxs = [ 0 .. numGuests - 1 ]
let tableIdxs = [ 0 .. numTables - 1 ]

let rec pairs l = seq {  
        match l with 
        | h::t -> for elem in t do yield h, elem
                  yield! pairs t
        | _ -> () }

let guestPairs = guestIdxs |> pairs |> List.ofSeq

let guestAtTableVars = solver.MakeBoolVarMatrix(numGuests, numTables, "GuestAtTable")
let guestAtTable guest table = guestAtTableVars.[guest, table]
let tableVarsForGuest guest =
    [ 0 .. numTables - 1 ] |> List.map (fun i -> guestAtTable guest i)
let guestVarsForTable table =
    [ 0 .. numGuests - 1 ] |> List.map (fun i -> guestAtTable i table)

let maleMajorityVars   = solver.MakeBoolVarArray(numTables, "MaleMajority")
let femaleMajorityVars = solver.MakeBoolVarArray(numTables, "FemaleMajority")

let guestPairsVars =
    tableIdxs
    |> List.map (fun t -> solver.MakeBoolVarMatrix(numGuests, numGuests))
    |> Array.ofList

let sitTogetherVar guest1 guest2 table =
    guestPairsVars.[table].[guest1,guest2]

let knowsSlackVars = solver.MakeNumVarArray(numGuests, 0.0, 10.0, "KnowsSlackVar")

let seatingCons =
    let makeSeatingConstraint guest =
        let lhs = tableVarsForGuest guest
                  |> LinearExpr.sum
        LinearExpr.op_Equality(lhs, 1.0)

    guestIdxs
    |> List.map makeSeatingConstraint
    |> List.map solver.Add

let tableMaxCons =
    let makeTableMaxConstraint table =
        let lhs = tableVarsForGuest table
                  |> LinearExpr.sum
        LinearExpr.op_LessThanOrEqual(lhs, float tableSeats)

    tableIdxs
    |> List.map makeTableMaxConstraint
    |> List.map solver.Add

let pairsSitTogetherCons =
    let pairSitsTogetherAllTables (g1,g2) =
        (tableVarsForGuest g1, tableVarsForGuest g2)
        ||> List.map2 (fun v1 v2 -> v1 - v2)
        |> List.map (fun lhs -> LinearExpr.op_Equality(lhs, 0.0))
    
    Data.pairsIdxs
    |> List.collect pairSitsTogetherAllTables
    |> List.map solver.Add

let majorityCons maleFac femaleFac (majorityVars : Variable[]) =
    let maleFemaleSum tableIdx =
        guestVarsForTable tableIdx
        |> List.mapi (fun i v -> match guestByIdx i with
                                 | Male   -> v * maleFac
                                 | Female -> v * femaleFac)
        |> List.reduce (+)
    let constraintByTable tableIdx =
        let lhs = (maleFemaleSum tableIdx) - majorityVars.[tableIdx]
        LinearExpr.op_LessThanOrEqual(lhs, 2.0)

    tableIdxs
    |> List.map constraintByTable
    |> List.map solver.Add

let maleMajorityCons   = majorityCons 1.0 -1.0 maleMajorityVars
let femaleMajorityCons = majorityCons -1.0 1.0 femaleMajorityVars

let sitTogetherCons =
    let consSet i j t =
        let con1 =
            let lhs = (sitTogetherVar i j t) - (guestAtTable i t)
            LinearExpr.op_LessThanOrEqual(lhs, 0.0)
        let con2 =
            let lhs = (sitTogetherVar i j t) - (guestAtTable j t)
            LinearExpr.op_LessThanOrEqual(lhs, 0.0)
        let con3 =
            let lhs = (sitTogetherVar i j t) - (guestAtTable i t) - (guestAtTable j t)
            LinearExpr.op_GreaterThanOrEqual(lhs, -1.0)
        [con1;con2;con3]
    let consPerTable tableIdx =
        guestPairs
        |> List.collect (fun (i,j) -> consSet i j tableIdx)
    tableIdxs
    |> List.collect consPerTable
    |> List.map solver.Add

let knowsCons =
    let consPerGuest guestIdx =
        let knowsVar = knowsSlackVars.[guestIdx]
        let conPerTable tableIdx =
            let atTableVar = guestAtTableVars.[guestIdx, tableIdx]
            let sum = guestIdxs
                      |> List.filter (knowsGuest guestIdx)
                      |> List.map (fun g2 -> sitTogetherVar guestIdx g2 tableIdx)
                      |> LinearExpr.sum
            let lhs = knowsVar - (3.0 * atTableVar) + sum
            LinearExpr.op_GreaterThanOrEqual(lhs, 0.0)
        tableIdxs
        |> List.map conPerTable
    guestIdxs
    |> List.collect consPerGuest
    |> List.map solver.Add

let objFunc =
    let first =
        let forTable t =
            guestPairs
            |> List.map (fun (i,j) -> (C_ij i j) * (sitTogetherVar i j t))
            |> List.reduce (+)
        tableIdxs   
        |> List.map forTable
        |> List.reduce (+)
    let second = 
        femaleMajorityVars
        |> List.ofArray
        |> LinearExpr.sum
    let third =
        maleMajorityVars
        |> List.ofArray
        |> LinearExpr.sum
    let fourth =
        knowsSlackVars
        |> Array.map (fun k -> 10.0 * k)
        |> Array.reduce (+)
    first + second + third + fourth

let s = solver.ExportModelAsLpFormat(false)