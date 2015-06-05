open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

type FooBuilder() =
    member __.Yield(()) : unit = invalidOp ""

    [<CustomOperation("one")>]
    member __.One((), thing : 'T) : unit = invalidOp "not implemented"

    [<CustomOperation("two")>]
    member __.Two((), thing : 'T) : unit = invalidOp "not implemented"

    member __.Quote expr : Expr<unit> = expr

    //member __.Run (expr : Expr<#seq<'Type>>) = expr
        //System.Console.WriteLine(expr)

let foo = FooBuilder()

let (*) (x : int list) (y : int list) = invalidOp "not implemented"
let inline (<=) x y = invalidOp "not implemented"

let bar = [1; 2; 3]
foo {
    one ((bar * bar) <= 0)
}

let insideLet() =
    let bar = [1; 2; 3]
    foo {
        one ((bar * bar) <= 0)
        two (bar)
    }

let x = insideLet()

let test =
    let rec inner l = 
        match x with
        | SpecificCall <@@ foo.Two @@> (_,_,exprList) -> 
            printfn "%A" exprList
            l :: [1.0]
        | _ -> l :: [0.0]
    inner []

(*
let makeConstraint3 (g1,g2) =
    let tableVars1 = tableIdxs |> Array.map (s g1)
    let tableVars2 = tableIdxs |> Array.map (s g2)
    (tableVars1, tableVars2) 
    ||> Array.mapi2 (fun t v1 v2 -> t, cplex.ScalProd([|1; -1|], [|v1; v2|]))
    |> Array.map (fun (t,expr) -> cplex.AddEq(expr, 0.0, sprintf "Constraint 3 for pair (%i,%i) on table %i" g1 g2 t))

let constraint3 = Data.pairsIdxs |> List.filter (fun (g1,g2) -> g1 < guestSubsetUsed && g2 < guestSubsetUsed) |> Array.ofList |> Array.collect makeConstraint3

let makeConstraint45 (maleFac : float) (femaleFac : float) (majorityVars : IIntVar[]) =
    let maleFemaleSum tableIdx =
        guestsAtTable tableIdx
        |> Array.mapi (fun i v -> match guestByIdx i with
                                  | Male   -> cplex.Prod(v |> toExpr, maleFac)
                                  | Female -> cplex.Prod(v |> toExpr, femaleFac))
        |> cplex.Sum
    
    let constraintByTable tableIdx =
        let lhs = 
            let t = [|
                cplex.ScalProd([| majorityVars.[tableIdx] |], [| -1 |]) :> INumExpr;
                (maleFemaleSum tableIdx)
                |]
            cplex.Sum(t)
        cplex.AddLe(lhs, 2.0, "Majority constraint for table " + tableIdx.ToString())

    tableIdxs
    |> Array.map constraintByTable

let maleMajorityVars = //cplex.BoolVarArray(numTables,tableIdxs |> Array.map (sprintf "Male majority at table %i"))
    cplex.IntVarArray(numTables, 0, System.Int32.MaxValue, tableIdxs |> Array.map (sprintf "Male majority at table %i"))
let femaleMajorityVars = //cplex.BoolVarArray(numTables,tableIdxs |> Array.map (sprintf "Female majority at table %i"))
    cplex.IntVarArray(numTables, 0, System.Int32.MaxValue, tableIdxs |> Array.map (sprintf "Female majority at table %i"))
//let constraint4 = makeConstraint45 1.0 -1.0 maleMajorityVars
//let constraint5 = makeConstraint45 -1.0 1.0 femaleMajorityVars
*)