// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open ProceduralStateMachine
open System.Collections.Generic

type C<'c> = | List of 'c list

module Tuple =
    let mapSnd f (a, b) = a, f b

    let mapFst f (a, b) = f a, f

    let map f1 f2 (a, b) = f1 a, f2 b

    let add1 a b = a, b

module Cmd =
    let map f (List l) = List (List.map f l)

    let none = List []

type Event =
    | Type1 of int
    | Type2 of string

type M<'s, 'e, 'c> = {
    init: 's * C<'c>
    update: 'e -> 's -> 's * C<'c>
    }

module Proc =
    type ProcedureResult<'a> =
        | Incomplete
        | Canceled
        | Completed of 'a

    type ProcedureState<'s, 'a> = 's * ProcedureResult<'a>

    type ProcedureEvent<'e> =
        | Timeout
        | MachineEvent of 'e

    type ProcedureCommand<'c> =
        | DoNothing
        | StartTimer
        | MachineCommand of 'c

    type Context<'s, 'e, 'c> = {
        machine: M<'s, 'e, 'c>
        state: 's
        command: C<'c>
        }

    type P<'s, 'e, 'c, 'a> = P of (M<'s, 'e, 'c> -> M<ProcedureState<'s, 'a>, ProcedureEvent<'e>, ProcedureCommand<'c>>)

    let retn a =
        let f m = { 
            init = m.init |> Tuple.map (Tuple.add1 (Completed a)) (Cmd.map MachineCommand)
            update = fun e s -> s, Cmd.none
            }
        P f

    let bind f (P p) =
        let f m = 
            async {
                let! cnew, a = p c
                let (P p2) = f a
                return! p2 cnew
                }
        P f

    let waitForEvent predicate =
        let run context =
            async {
                let mutable state = context.state
                let mutable command = context.command
                let mutable result = Option.None
                while result.IsNone do
                    context.processor(command);
                    let! event = context.events ()
                    result <- 
                        match event with
                        | Timeout -> result
                        | MachineEvent e -> predicate e
                    if result.IsNone then
                        let (newState, newCommand) = context.machine.update event context.state
                        state <- newState
                        command <- 
                            match newCommand with
                            | StartTimer
                return { context with state = state; command = command }, result.Value
                }
        P run

    let waitForCommand predicate =
        let run context =
            async {
                let mutable state = context.state
                let mutable command = context.command
                let mutable result = None
                while result.IsNone do
                    result <- predicate command
                    if result.IsNone then
                        context.processor(command);
                        let! event = context.events ()
                        let (newState, newCommand) = context.machine.update event context.state
                        state <- newState
                        command <- newCommand
                return { context with state = state; command = command }, result.Value
                }
        P run

    let runProc (P p) m events processor =
        let (s, c) = m.init
        async {
            let! result = p { state = s; command = c; machine = m; events = events; processor = processor }
            return snd result
            }

type ProcedureBuilder() =
    member x.Bind(p, f) = Proc.bind f p
    member x.Return(v) = Proc.retn v

let proc = new ProcedureBuilder()

type Command = string

let eventType1 event =
    match event with | Type1 i -> Some i | _ -> None

let machine = {
    init = 0, "initialize"
    update = fun (e: Event) s -> s + 1, "command" }

let p1 = Proc.bind (fun e1 -> Proc.retn e1) (Proc.waitForEvent eventType1)

let events = fun () -> async { return Type2 "124" }

let processor c = printf "%A" c

let r1 = Proc.runProc p1 machine events processor

let p2 = proc {
    let! e1 = Proc.waitForEvent eventType1
    return e1
    }

let r2 = Proc.runProc p2 machine events processor

let output = Async.RunSynchronously (r2, 100)
