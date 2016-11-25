
type Position = int

type Stream<'e> = Position * StreamNext<'e>

and StreamNext<'e> = 
    | End
    | Remainder of 'e * (unit -> Stream<'e>)

type Severity = Error | Warning | Success

type Detail<'e> =
    | Empty
    | And of Detail<'e> list
    | Or of Detail<'e> list
    | List of Detail<'e> list
    | Summary of Severity * string * 'e * Detail<'e>

let andDetail d1 d2 =
    match (d1, d2) with
    | Empty, Empty -> Empty
    | Empty, d2 -> d2
    | d1, Empty -> d1
    | And list1, And list2 -> And (List.concat [list1; list2])
    | And list1, d2 -> And (List.concat [list1; [d2]])
    | d1, And list -> And (d1 :: list)
    | _ -> And [d1; d2]

let orDetail d1 d2 =
    match (d1, d2) with
    | Empty, Empty -> Empty
    | Empty, d2 -> d2
    | d1, Empty -> d1
    | Or list1, Or list2 -> Or (List.concat [list1; list2])
    | Or list1, d2 -> Or (List.concat [list1; [d2]])
    | d1, Or list -> Or (d1 :: list)
    | _ -> Or [d1; d2]

type Parser<'e, 'd, 'r> = 
    Stream<'e> -> Result<'e, 'd, 'r>

and Result<'e, 'd, 'r> =
    | Match of 'r * Detail<'d> * Stream<'e>
    | NoMatch of Detail<'d>

let bind f p =
    fun s -> 
        match p s with
        | NoMatch d -> NoMatch d
        | Match (r1, d1, s1) -> 
            match (f r1) s1 with
            | NoMatch d2 -> NoMatch (andDetail d1 d2)
            | Match (r2, d2, s2) -> Match (r2, andDetail d1 d2, s2)

let retn r s =
    Match (r, Empty, s)

let map f p =
    fun s -> 
        match p s with
        | NoMatch d -> NoMatch d
        | Match (r, d, s) -> Match (f r, d, s)

let (<|>) p1 p2 =
    fun s ->
        match p1 s with
        | Match _ as m -> m
        | NoMatch d1 -> 
            match p2 s with
            | Match (r2, d2, snext) -> Match (r2, (orDetail d1 d2), snext)
            | NoMatch d2 -> NoMatch (orDetail d1 d2)

let where predicate p =
    fun s -> 
        match p s with
        | Match (r, d, snext) as m -> 
            match predicate r with
            | true -> m
            | false -> NoMatch d
        | NoMatch d -> NoMatch d

let mapDetail f p =
    fun s ->
        match p s with
        | NoMatch d -> NoMatch d
        | Match (r, d, snext) -> 
            let (severity, message) = f r
            Match (r, Summary (severity, message, s, Empty), snext)

let check predicate message =
    mapDetail (fun r -> (if predicate r then Success else Error), message)

let checkMatch message p =
    fun s -> 
        match p s with
        | Match (r, d, snext) -> Match (r, (Summary (Success, message, s, d)), snext)
        | NoMatch d -> NoMatch (Summary (Error, message, s, d))

type ParserBuilder() =
    member x.Bind(p, f) = bind f p
    member x.Return(r) = retn r
    member x.ReturnFrom(p) = p

let criteria = ParserBuilder()

let any = function
    | pos, End as s -> NoMatch Empty
    | pos, Remainder (e, s) -> Match (e, Empty, s ())

let cnst e = any |> where ((=) e)

module List =
    let toStream l =
        let rec toStreamI pos = function
            | [] -> pos, End
            | head :: tail -> pos, Remainder (head, fun () -> toStreamI (pos + 1) tail)
        toStreamI 0 l

let c1 = criteria {
    let! e1 = 
        any 
        |> where (fun e -> e = 1 || e = 2)
        |> check ((=) 1) "Must be a 1"

    let! e2 = (cnst 2 <|> cnst 0) |> check ((=) 2) "Must be a 2"
    let! e3 = any |> check ((=) 3) "Must be a 3"
    return e1, e2, e3
}

let cOr = 
    (cnst 0 |> checkMatch "expecting a 0")
    <|> 
    (cnst 1 |> checkMatch "expecting a zero")

List.toStream [1; 2; 3] |> c1

List.toStream [1; 1; 3] |> c1

List.toStream [2; 0; 3] |> c1

List.toStream [3; 2; 3] |> c1

List.toStream [3] |> cnst 2

List.toStream [1] |> cOr

List.toStream [2] |> (cnst 1 |> checkMatch "Expected a match")