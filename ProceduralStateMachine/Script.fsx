// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open ProceduralStateMachine
open System.Collections.Generic

type C<'c> = | List of 'c list

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
    type TimerId = int

    type ProcedureEvent<'e> =
        | Cancel
        | Timeout of TimerId
        | MachineEvent of 'e

    type ProcedureCommand<'c> =
        | DoNothing
        | StartTimer
        | MachineCommand of 'c

    type Context<'s, 'e, 'c> = {
        nextTimerId: TimerId
        machine: M<'s, 'e, 'c>
        state: 's
        command: 'c
        events: unit -> Async<ProcedureEvent<'e>>
        processor: 'c -> unit
        }

    type Result<'a> =
        | Cancelled
        | Timedout of TimerId
        | Exception of System.Exception
        | Completed of 'a

    type P<'s, 'e, 'c, 'a> = 
        P of (Context<'s, 'e, 'c> -> Async<Context<'s, 'e, 'c> * Result<'a>>)

    let retn a =
        let run c = async { return c, Completed a }
        P run

    let bind f (P p) =
        let run c = 
            async {
                let! cnew, a = p c
                let (P p2) = f a
                return! p2 cnew
                }
        P run

    let waitForEvent predicate =
        let run context =
            async {
                let mutable state = context.state
                let mutable command = context.command
                let mutable result = None
                while result.IsNone do
                    context.processor(command);
                    let! event = context.events ()
                    result <- 
                        match event with
                        | Cancel -> Some Cancelled
                        | Timeout id -> Some (Timedout id)
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