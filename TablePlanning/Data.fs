module Data

open FSharp.Data
open System.IO
open Domain

[<Literal>]
let guestPath = @"C:\Users\krist_000\Documents\Visual Studio 2013\Projects\TablePlanning\guest.data"
[<Literal>]
let pairsPath = @"C:\Users\krist_000\Documents\Visual Studio 2013\Projects\TablePlanning\pairs.data"
[<Literal>]
let groupsPath = @"C:\Users\krist_000\Documents\Visual Studio 2013\Projects\TablePlanning\groups.data"

type Guests = CsvProvider<guestPath>
type Pairs = CsvProvider<pairsPath>

let private guests : Person list = 
    let rowToPerson (row : Guests.Row) =
        let gender = match row.Gender with
                     | false -> Female
                     | true -> Male
        let interests = row.Interests.Split([|';'|])
                        |> List.ofArray
                        |> List.map (fun s -> s.Trim())
                        |> List.filter (fun s -> s.Length > 0)
                        |> List.map Interest
        Person(Name(row.Name), gender, Age(row.Age), interests)
    
    Guests.Load(guestPath).Rows
    |> Seq.map rowToPerson
    |> List.ofSeq

let totalGuests = guests |> List.length
let personFromName name = (guests |> List.tryFind (fun p -> p.Name.Value = name)).Value


let private findGuest name =
    let opt = 
        guests
        |> List.tryFind (fun x -> x.Name = Name(name))
    match opt with
    | Some(guest) -> guest
    | None -> failwith ("Cannot find guest " + name)
    
let peopleInPairs =
    Pairs.Load(pairsPath).Rows
    |> Seq.collect (fun row -> [findGuest (row.Guest1.Trim()); findGuest (row.Guest2.Trim())])
    |> Set.ofSeq

let isInPair person = peopleInPairs.Contains(person)

let visitors =
    let singles =
        guests
        |> List.filter (isInPair >> not)
        |> List.map Single
    
    let pairs =
        Pairs.Load(pairsPath).Rows
        |> Seq.map (fun row -> Pair(findGuest (row.Guest1.Trim()),findGuest (row.Guest2.Trim())))
        |> List.ofSeq

    singles @ pairs

let visitorIdx v =
    match List.tryFindIndex((=) v) visitors with
    | Some(i) -> i
    | None -> failwith "Cannot find visitor %A" v

let visitorFromIdx i = List.nth visitors i

let visitorFromPerson person =
    let containsPerson = function
                         | Single(p) when p = person -> true
                         | Pair(p1,p2) when (p1 = person || p2 = person) -> true
                         | _ -> false
    let foundOpt = visitors |> List.tryFind containsPerson
    match foundOpt with
    | Some(v) -> v
    | None -> failwith "Cannot find person %A" person

let personToVisitorIdx = visitorFromPerson >> visitorIdx
               
let groups = 
    let groupRowToGroup (row : string) : Group =
        row.Split([|','|])
        |> Seq.map (fun s -> s.Trim())
        |> Seq.map findGuest
        |> List.ofSeq
    
    groupsPath
    |> File.ReadAllLines
    |> List.ofArray
    |> List.map groupRowToGroup

let private knows =
    let rec pairs l = seq {  
        match l with 
        | h::t -> for elem in t do yield h, elem
                  yield! pairs t
        | _ -> () }
    
    let numGuests = visitors |> List.length
    let res = [| 0 .. numGuests - 1 |]
              |> Array.map (fun _ -> Array.init numGuests (fun _ -> false))

    let setKnowsForGroup group =
        pairs group
        |> Seq.map (fun (p1, p2) -> personToVisitorIdx p1, personToVisitorIdx p2)
        |> Seq.iter (fun (idx1,idx2) -> res.[idx1].[idx2] <- true
                                        res.[idx2].[idx1] <- true)

    groups
    |> List.iter setKnowsForGroup

    res

let knowsVisitor i j = knows.[i].[j]

let C_ij i j = visitorScore (visitorFromIdx i) (visitorFromIdx j)

let W_i i = visitorWeight (visitorFromIdx i)