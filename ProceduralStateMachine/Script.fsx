// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open ProceduralStateMachine
open System.Collections.Generic
open System

fsi.ShowDeclarationValues <- false

type C<'c> = | List of 'c list

module Cmd =
    let map f (List l) = List (List.map f l)

    let none = List []

    let one c = List [c]

    let add cmd (List l) = List (cmd :: l)

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
        | StartTimer of TimerId * System.TimeSpan
        | Automation of string * ((string * string) list)
        | MachineCommand of 'c

    type Context<'s, 'e, 'c> = {
        nextTimerId: TimerId
        waitTimer: TimerId
        machine: M<'s, 'e, 'c>
        state: 's
        command: C<ProcedureCommand<'c>>
        events: unit -> Async<ProcedureEvent<'e>>
        processor: C<ProcedureCommand<'c>> -> unit
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
                return! 
                    match a with
                    | Cancelled -> async { return c, Cancelled }
                    | Timedout id -> async { return c, Timedout id }
                    | Exception ex -> async { return c, Exception ex }
                    | Completed a -> 
                        let (P p2) = f a
                        p2 cnew
            }
        P run

    let updateContext predicate context procEvent =
        match procEvent with
        | Cancel -> context, Some Cancelled
        | Timeout id -> 
            context, 
            // Ignore timer expirations for procedures that have already completed
            if id = context.waitTimer then Some (Timedout id) 
            else None
        | MachineEvent e -> 
            match predicate e with
            | None -> 
                let (newState, newCommand) = context.machine.update e context.state
                { context with state = newState; command = Cmd.map MachineCommand newCommand }, None
            | Some a -> context, Some (Completed a)

    let contextStartTimer duration context =
        { context with 
            nextTimerId = context.nextTimerId + 1
            command = Cmd.add (ProcedureCommand.StartTimer (context.nextTimerId, duration)) context.command }

    let contextProcessCommand context =
        context.processor context.command
        { context with command = Cmd.none }

    let waitForEvent duration predicate: P<'state, 'event, 'command, 'result> =
        let run context =
            async {
                let mutable ctx = contextStartTimer duration context
                let mutable result = None
                while result.IsNone do
                    ctx <- contextProcessCommand ctx
                    let! event = context.events ()
                    let (newCtx, newResult) = updateContext predicate ctx event
                    ctx <- newCtx
                    result <- newResult
                return ctx, result.Value
            }
        P run

    let onEvent duration selectCont: P<'state, 'event, 'command, 'result> =
        let run context =
            async {
                let mutable ctx = contextStartTimer duration context
                let mutable result = None
                while result.IsNone do
                    ctx <- contextProcessCommand ctx
                    let! event = context.events ()
                    let (newCtx, newResult) = updateContext selectCont ctx event
                    ctx <- newCtx
                    result <- newResult
                return ctx, result.Value
            }
        bind id (P run)

    let runProc (P p) m events processor =
        let (s, c) = m.init
        async {
            let context = { 
                state = s
                command = Cmd.map MachineCommand c
                machine = m
                events = events
                processor = processor
                nextTimerId = 0
                waitTimer = 0 
            }
            let! result = p context
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
    init = 0, Cmd.one "initialize"
    update = fun (e: Event) s -> 
        printf "update called\n"
        s + 1, Cmd.one "command " }

let p1 = Proc.bind Proc.retn (Proc.waitForEvent (System.TimeSpan.FromSeconds 1.0) eventType1)

let mutable events = [ 
    Proc.MachineEvent (Type1 123)
    Proc.MachineEvent (Type2 "124")
    Proc.MachineEvent (Type2 "124")
    Proc.MachineEvent (Type2 "124")
    Proc.MachineEvent (Type2 "124")
    Proc.MachineEvent (Type2 "124")
    Proc.MachineEvent (Type2 "124")
    Proc.MachineEvent (Type2 "124")
    Proc.MachineEvent (Type2 "124")
    Proc.MachineEvent (Type1 456)
    Proc.Timeout 0
]

let nextEvent () = async { 
    let r = events.Head
    events <- events.Tail
    return r
}

let processor c = printf "processing command: %A\n" c

let r1 = Proc.runProc p1 machine nextEvent processor

let onEventType2 timeout proc =
    Proc.onEvent timeout (function | Type2 v -> Some (proc v) | _ -> None)

let event2Handler (event: string) = proc { return event.Length }

let p2 = proc {
    let timeout = TimeSpan.FromSeconds 1.0

    let! e1 = Proc.waitForEvent timeout eventType1

    let! e2 = onEventType2 timeout event2Handler

    let! e3 = 
        Proc.onEvent timeout (function 
            | Type1 e1 -> Some (proc { return "Got a type1 event" })
            //| Type2 e2 -> Some (Proc.retn "Got a type2 event")
            | _ -> None)

    return e1, e2, e3
    }

//let r2 = Proc.runProc p2 machine nextEvent processor

//let output = Async.RunSynchronously (r2, 1000)

//printf "result = %A\n" output

