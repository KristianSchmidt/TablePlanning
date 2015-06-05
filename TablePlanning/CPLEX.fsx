#r @"../lib/ILOG.CPLEX.dll"
#r @"../lib/ILOG.Concert.dll"
#r @"..\packages\FSharp.Data.2.1.1\lib\net40\FSharp.Data.dll"

#load "Domain.fs"
#load "Data.fs"

open Domain
open Data
open ILOG.Concert
open ILOG.CPLEX

module CPLEXHelpers =
    let cplex = new Cplex();

    /// Creates an INumExpr which is the first var minus the other
    let sub (v1 : IIntVar) (v2 : IIntVar) =
        cplex.ScalProd([|v1;v2|], [|1;-1|]) :> INumExpr

    // Creates an INumExpr which is the first var minus the two others
    let sub3 (v1 : IIntVar) (v2 : IIntVar) (v3 : IIntVar) =
        cplex.ScalProd([|v1;v2;v3|], [|1;-1;-1|]) :> INumExpr

    /// Creates an array of INumExpr, translating arrays [v1,v2,v3,...] and [u1,u2,u3,...] to [v1-u1,v2-u2,v3-u3,...]
    let subArr (a1 : IIntVar[]) (a2 : IIntVar[]) =
        (a1,a2) ||> Array.map2 sub

    /// Creates a sum of variables as an INumExpr
    let sum (elements : IIntVar[]) =
        let len = elements.Length
        cplex.ScalProd(elements, Array.create len 1) :> INumExpr

    let toExpr var = cplex.Prod(var, 1)

    let prod (i : int) var = cplex.Prod(var, i)

    let forceVarTo1Constraint (var : IIntVar) = 
        cplex.AddEq(var |> toExpr, 1.0)

    let export() = 
        let exportFilename = @"C:\Users\krist_000\Documents\Visual Studio 2013\Projects\TablePlanning\model.lp"
        cplex.ExportModel(exportFilename)
        
open CPLEXHelpers

module Model =
    [<AutoOpen>]
    module Config =
        /// The typed visitors
        let visitors = Data.visitors |> Array.ofSeq
        
        /// The number of visitors
        let numGuests = visitors |> Array.length
        /// Indices for all of the guests
        let guestIdxs = [| 0 .. numGuests - 1 |]

        /// Number of tables
        let numTables = 9
        /// Indices for all of the available tables
        let tableIdxs = [| 0 .. numTables - 1 |]

        /// Number of seats per table
        let tableSeats = 8

    module Vars =
        /// The boolean variables which correspond to s_{i,t} in the paper
        let guestAtTableVars =
            let makeArrayForVisitor (visitor : Visitor) =
                let names = tableIdxs |> Array.map (sprintf "%s at table %i" visitor.Name)
                cplex.BoolVarArray(numTables, names)
            visitors |> Array.map makeArrayForVisitor

        let private sitTogetherVars =
            Array.init numGuests (fun i -> Array.init numGuests (fun j -> Array.init numTables (fun t -> cplex.BoolVar(sprintf "Guests %i and %i sit at table %i" i j t))))    

        let s i t = guestAtTableVars.[i].[t]
        
        let t i j t = sitTogetherVars.[i].[j].[t]

open Model
open Model.Config
open Model.Vars

let rec pairs l = seq {  
        match l with 
        | h::t -> for elem in t do yield h, elem
                  yield! pairs t
        | _ -> () }

let guestPairs = guestIdxs |> List.ofArray |> pairs |> Array.ofSeq

/// Constraint 1, which denotes that a guest should be placed at exactly one table
let makeConstraint1 (visitor : Visitor) =
    let equalsOne expr = cplex.AddEq(expr, 1.0, sprintf "%s can only be placed at one table" visitor.Name)
    let visitorIdx = visitorIdx visitor
    tableIdxs |> Array.map (s visitorIdx) |> sum |> equalsOne

let constraint1 = visitors |> Array.map makeConstraint1

/// Constraint 2, which denotes that a table can hold at max _tableSeats_ guests
let makeConstraint2 tableIdx =
    let lessThanTableSeats expr = cplex.AddLe(expr, float tableSeats, sprintf "Table %i can only hold %i people" tableIdx tableSeats)
    guestIdxs
    |> Array.map (fun v -> (s v tableIdx) |> prod (W_i v))
    |> cplex.Sum
    |> lessThanTableSeats

let constraint2 = tableIdxs |> Array.map makeConstraint2

let sitTogetherCons =
    let consSet i j tbl =
        let con1 =
            let lhs = sub (t i j tbl) (s i tbl)
            cplex.AddLe(lhs, 0.0, sprintf "Con 6 for (%i,%i,%i)" i j tbl)
        let con2 =
            let lhs = sub (t i j tbl) (s j tbl)
            cplex.AddLe(lhs, 0.0, sprintf "Con 7 for (%i,%i,%i)" i j tbl)
        let con3 =
            let lhs = sub3 (t i j tbl) (s i tbl) (s j tbl)
            cplex.AddGe(lhs, -1.0, sprintf "Con 8 for (%i,%i,%i)" i j tbl)
        [|con1;con2;con3|]
    let consPerTable tableIdx =
        guestPairs
        |> Array.collect (fun (i,j) -> consSet i j tableIdx)
    tableIdxs
    |> Array.collect consPerTable

let knowsVars =
    cplex.NumVarArray(numGuests, Array.init numGuests (fun _ -> 0.0), Array.init numGuests (fun _ -> 10.0), guestIdxs |> Array.map (sprintf "KnowsVar%i"))

let knowsCons =
    let knowsPerGuest visitorIdx =
        let knowsVar = knowsVars.[visitorIdx]
        let conPerTable tblIdx =
            let part1 = knowsVar :> INumExpr
            let part2 = cplex.ScalProd([|(s visitorIdx tblIdx)|], [| -2 |]) :> INumExpr
            let part3 = 
                guestIdxs
                |> Array.filter (knowsVisitor visitorIdx)
                |> Array.map (fun v2 -> t visitorIdx v2 tblIdx)
                |> Array.map (fun v -> v :> INumExpr)
                |> cplex.Sum
            cplex.AddGe( cplex.Sum([|part1;part2;part3|]), 0.0, sprintf "Constraint 9 for guest %i table %i" visitorIdx tblIdx)
        tableIdxs
        |> Array.map conPerTable
    guestIdxs
    |> Array.collect knowsPerGuest

let obj =
    let part1 =
        guestPairs
        |> Array.collect (fun (v1,v2) -> tableIdxs |> Array.map (fun t -> (v1,v2,t)))
        |> Array.map (fun (v1,v2,tbl) -> cplex.Prod(C_ij v1 v2, t v1 v2 tbl))
        |> cplex.Sum
    (*
    let part2 =
        femaleMajorityVars
        |> Array.map toExpr
        |> cplex.Sum :> INumExpr
    let part3 =
        maleMajorityVars
        |> Array.map toExpr
        |> cplex.Sum :> INumExpr
    *)
    let part4 =
        knowsVars
        |> Array.map (fun v -> cplex.Prod(10.0, v))
        |> cplex.Sum
    cplex.Sum [|part1;part4|]
    //cplex.Sum [|part4|]

cplex.AddObjective(ObjectiveSense.Minimize, obj)

//CPLEXHelpers.export()

// Anti symmetry constraints
s (visitorIdx (Pair(personFromName "Kristian", personFromName "Lea"))) 0 |> forceVarTo1Constraint
s (visitorIdx (Pair(personFromName "Rasmus", personFromName "Mona"))) 1 |> forceVarTo1Constraint
s (visitorIdx (Pair(personFromName "Katrine", personFromName "Johannes"))) 2 |> forceVarTo1Constraint
s (visitorIdx (Pair(personFromName "Adam", personFromName "Christina"))) 3 |> forceVarTo1Constraint
s (visitorIdx (Pair(personFromName "Christel", personFromName "Børge"))) 4 |> forceVarTo1Constraint
s (visitorIdx (Pair(personFromName "Signe", personFromName "Signes mand Henrik"))) 5 |> forceVarTo1Constraint
s (visitorIdx (Single(personFromName "Nina"))) 6 |> forceVarTo1Constraint
s (visitorIdx (Single(personFromName "Jette"))) 7 |> forceVarTo1Constraint
s (visitorIdx (Pair(personFromName "Louis", personFromName "Inga"))) 8 |> forceVarTo1Constraint

cplex.SetParam(Cplex.IntParam.CutPass, -1)
cplex.Solve()

let guestIsAtTable gIdx tIdx =
    match (cplex.GetValue(s gIdx tIdx)) with
    | 1.0 -> Some(tIdx)
    | _ -> None

let tableForGuest gIdx =
    tableIdxs |> Array.pick (guestIsAtTable gIdx)



guestIdxs
|> Array.map (fun g -> (visitorFromIdx g).Name, tableForGuest g)
|> Seq.groupBy (fun (name,t) -> t)
|> Seq.map (fun (t,inner) -> t, inner |> Seq.map fst |> List.ofSeq)
|> Seq.sortBy fst
|> List.ofSeq

cplex.SetParam(Cplex.IntParam.TuningDisplay, 2)
cplex.TuneParam()