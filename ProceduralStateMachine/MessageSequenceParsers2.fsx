
(*
    Try to integrate Detail and Result, as compared to MessageSequenceParsers.fsx
 *)

type Position = int

type Stream<'e> = Position * StreamNext<'e>

and StreamNext<'e> = 
    | End
    | Remainder of 'e * (unit -> Stream<'e>)

type Severity = Error | Warning | Success

type Result<'e, 'd, 'r> =
    | Match of 'r * Stream<'e> * Detail<'e, 'd>
    | NoMatch of Detail<'e, 'd>

and Detail<'e, 'd> =
    | Empty
    | And of AndDetail<'e, 'd>
    | Or of OrDetail<'e, 'd>
    | Where of WhereDetail<'e, 'd>
    | Summary of 'd * Stream<'e> * Detail<'e, 'd>

and AndDetail<'e, 'd> =
    | LeftFail of Detail<'e, 'd>
    | RightFail of Detail<'e, 'd> * Detail<'e, 'd>
    | BothMatch of Detail<'e, 'd> * Detail<'e, 'd>

and OrDetail<'e, 'd> =
    | LeftMatch of Detail<'e, 'd>
    | RightMatch of Detail<'e, 'd> * Detail<'e, 'd>
    | BothFail of Detail<'e, 'd> * Detail<'e, 'd>

and WhereDetail<'e, 'd> =
    | Fail of Detail<'e, 'd>
    | PredicateFail of Detail<'e, 'd>
    | PredicateMatch of Detail<'e, 'd>

type Parser<'e, 'd, 'r> = 
    Stream<'e> -> Result<'e, 'd, 'r>

let bind f p =
    fun s -> 
        match p s with
        | NoMatch d -> NoMatch (And (LeftFail d))
        | Match (r1, s1, d1) -> 
            match (f r1) s1 with
            | NoMatch d2 -> NoMatch (And (RightFail (d1, d2)))
            | Match (r2, s2, d2) -> Match (r2, s2, And (BothMatch (d1, d2)))

let retn r s =
    Match (r, s, Empty)

let map f p =
    fun s -> 
        match p s with
        | NoMatch d -> NoMatch d
        | Match (r, d, s) -> Match (f r, d, s)

let (<|>) p1 p2 =
    fun s ->
        match p1 s with
        | Match (r, s, d) -> Match (r, s, Or (LeftMatch d))
        | NoMatch d1 -> 
            match p2 s with
            | Match (r2, snext, d2) -> Match (r2, snext, Or (RightMatch (d1, d2)))
            | NoMatch d2 -> NoMatch (Or (BothFail (d1, d2)))

let where predicate p =
    fun s -> 
        match p s with
        | Match (r, snext, d) -> 
            match predicate r with
            | true -> Match (r, snext, Where (PredicateMatch d))
            | false -> NoMatch (Where (PredicateFail d))
        | NoMatch d -> NoMatch (Where (Fail d))

let mapDetail f p =
    fun s ->
        match p s with
        | NoMatch d -> NoMatch d
        | Match (r, snext, d) -> 
            Match (r, snext, f d)

let check predicate message p =
    fun s ->
        match p s with
        | NoMatch d -> NoMatch d
        | Match (r, snext, d) -> 
            let summary = (if predicate r then Success else Error), message
            Match (r, snext, Summary (summary, s, d))

let checkMatch message p =
    fun s -> 
        match p s with
        | Match (r, snext, d) -> Match (r, snext, Summary ((Success, message), s, d))
        | NoMatch d -> NoMatch (Summary ((Error, message), s, d))

type ParserBuilder() =
    member x.Bind(p, f) = bind f p
    member x.Return(r) = retn r
    member x.ReturnFrom(p) = p

let criteria = ParserBuilder()

let any = function
    | pos, End as s -> NoMatch Empty
    | pos, Remainder (e, s) -> Match (e, s (), Empty)

let cnst e = any |> where ((=) e) |> checkMatch (sprintf "expected a %A" e)

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

List.toStream [1; 2; 3; 4] |> c1

List.toStream [1; 1; 3] |> c1

List.toStream [2; 0; 3] |> c1

List.toStream [3; 2; 3] |> c1

List.toStream [3] |> cnst 2

List.toStream [1] |> cOr

List.toStream [2] |> (cnst 1 |> checkMatch "Expected a match")