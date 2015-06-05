module Domain

type Name = | Name of name : string
            member this.Value = match this with | Name(n) -> n

type Gender = | Male
              | Female

type Age = | Age of age : int

type Interest = | Interest of interest : string

type Person = | Person of name : Name * gender : Gender * age : Age * interests : Interest list
              member this.Name = match this with | Person(name,_,_,_) -> name

type Visitor = | Single of Person
               | Pair of Person * Person
               member this.Name =
                 match this with
                 | Single(Person(name,_,_,_)) -> name.Value
                 | Pair(Person(n1,_,_,_),Person(n2,_,_,_)) -> sprintf "(%s,%s)" n1.Value n2.Value

type Group = Person list

let visitorWeight visitor =
    match visitor with
    | Single(_) -> 1
    | Pair(_)   -> 2

let visitorScore g1 g2 =
    let coef p1 p2 =
        match p1,p2 with
        | Person(_,_,Age(age1),interests1),Person(_,_,Age(age2),interests2) ->
            let commonInterests = interests1 |> List.filter (fun i -> List.exists ((=) i) interests2) |> List.length |> float
            let ageDiff = ((age1 - age2) |> abs |> float) / 10.0
            commonInterests - ageDiff
    match g1,g2 with
    | Single(p1),Single(p2) -> coef p1 p2
    | Pair(p1,p2),Single(p3)
    | Single(p3),Pair(p1,p2) -> coef p1 p3 + coef p2 p3
    | Pair(p1,p2),Pair(p3,p4) -> coef p1 p3 + coef p1 p4 + coef p2 p3 + coef p2 p4