
module Tuple =
    let mapSnd f (a, b) = (a, f b)

    let bimap f g (a, b) = (f a, g b)

    let addSnd b a = (a, b)

    let addFst a b = (a, b)

type Request<'e, 's> =
  { request: 'e
    source: 's }

module Request =
    let map f r = { request = r.request; source = f r.source }

type Cmd<'e, 'c> = 
    | Internal of Request<'c, 'e>
    | External of (('e -> unit) -> unit)

module Cmd =
    let none = External ignore

    let map (f: 'a -> 'b) cmd = 
        match cmd with
        | Internal req -> Internal (Request.map f req)
        | External run -> External (fun k' -> run (fun a -> k' (f a)))

    let req data source

type EventStack<'e, 'e2> =
  | Head of 'e
  | Tail of 'e2

type Machine<'e, 's, 'c> =
  { state: 's
    update: 'e -> 's -> 's * Cmd<'e, 'c> }

type Runtime<'e, 's> = Machine<'e, 's, 'e> -> unit

type RuntimeStack<'e, 'e2, 's, 's2> = Runtime<'e * 'e2, 's * 's2>

type MachineStack<'e, 'etail, 's, 'stail> = Machine<EventStack<'e, 'etail>, 's * 'stail, EventStack<'e, 'etail>>

let empty = 
  { state = ()
    update = fun () () -> ((), Cmd.none) }

let push machine stack =
  { state = machine.state, stack.state
    update = fun e (s, s') ->
        match e with
        | Head e1 -> machine.update e1 s |> Tuple.bimap (Tuple.addSnd s') (Cmd.map Head)
        | Tail e2 -> stack.update e2 s' |> Tuple.bimap (Tuple.addFst s) (Cmd.map Tail)
  }

let bottom r = push r empty

let rec step (event: EventStack<'e, 'etail>) (state: 's * 'stail) (stack: MachineStack<'e, 'etail, 's, 'stail>) =
    let (state', cmd) = stack.update event state
    match cmd with
    | Internal req -> step req.request state' stack
    | External run -> state' // Here is where we need to start external events and submit a callback to queue the completion
