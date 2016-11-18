// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open ProceduralStateMachine
open System.Collections.Generic
open System

fsi.ShowDeclarationValues <- false

// This is rather surprising (to me, at least), but the bool flag is needed here in order to know 
// whether the Co is created with Bind or Return.  If created with Return, it implies we aren't 
// waiting for further input.  It seems like this is redundant with the "Done/Next" cases in the 
// CoResult type, but it's not the same thing, apparently.
type Co<'s, 'r> = bool * ('s -> CoResult<'s, 'r>)
and CoResult<'s, 'r> = 
    | Done of 'r
    | Next of Co<'s, 'r>

type CoBuilder() =
    member x.Bind((b, co), f) =
        true, fun s ->
            match co s with
            | Done r -> Next (f r)
            | Next (b, n) -> Next (x.Bind((b, n), f))

    member x.Return(v): Co<'s, 'r> =
        false, (fun s -> Done v)

let co = CoBuilder()

let rec ifOdd s =
    match s % 2 = 0 with
    | true -> Done s
    | false -> Next (true, ifOdd)

let ifOddC = (true, ifOdd)

let cor = co { 
    let! fo = ifOddC
    return fo 
}

let co1 = co {
    let! isOdd = ifOddC
    let! isOdd2 = ifOddC
    let! v3 = cor
    return isOdd, isOdd2, v3
}

let co2 = 
    co.Bind(
        ifOddC, (fun isOdd -> 
            co.Bind(
                ifOddC, (fun isOdd2 -> 
                    co.Return(isOdd, isOdd2)))))

let step s next = 
    match next with
    | Done r -> Done r
    | Next (b, n) -> n s

(snd co1) 2 |> step 6 |> step 5

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
        event: 'e
        processor: C<ProcedureCommand<'c>> -> unit
    }

    type Result<'a> =
        | Cancelled
        | Timedout of TimerId
        | Exception of System.Exception
        | Completed of 'a

    type P<'s, 'e, 'c, 'a> = 
        //P of (Context<'s, 'e, 'c> -> Async<Context<'s, 'e, 'c> * Result<'a>>)
        P of ('e -> 's -> 's * C<ProcedureCommand<'c>> * Step<'s, 'e, 'c, 'a>)

    and Step<'s, 'e, 'c, 'a> =
        | Done of 'a
        | Next of P<'s, 'e, 'c, 'a>

    let retn a =
        P (fun e s -> s, Cmd.none, Done a)

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

let r2 = Proc.runProc p2 machine nextEvent processor

let output = Async.RunSynchronously (r2, 1000)

printf "result = %A\n" output

