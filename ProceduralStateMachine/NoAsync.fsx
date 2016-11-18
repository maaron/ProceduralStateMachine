// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open ProceduralStateMachine
open System.Collections.Generic
open System

fsi.ShowDeclarationValues <- false

module Seq =
    let takeUntil predicate (sequence: IEnumerable<'t>) =
        seq {
            use en = sequence.GetEnumerator()
            let mutable stop = false
            while not stop && en.MoveNext() do
                yield en.Current
                stop <- predicate en.Current
        }

module Proc =
    type Wait = Waiting | Ready

    type P<'s, 'a> = Wait * ('s -> R<'s, 'a>)

    and R<'s, 'a> = 
        | Done of 's * 'a
        | Next of 's * P<'s, 'a>

    type PBuilder() =
        member x.Bind(p: P<'s, 'a>, f: 'a -> P<'s, 'a2>): P<'s, 'a2> =
            let (wait, cont) = p
            let step state =
                match cont state with
                | Done (s, a) -> Next (s, f a)
                | Next (s, pnext) -> Next (s, x.Bind(pnext, f))
            (wait, step)

        member x.Return(v: 'a): P<'s, 'a> = 
            (Ready, fun s -> Done (s, v))

        member x.ReturnFrom(p) = p

    // This function recursively applies the current state until the procedure is either done 
    // or waiting for the next state
    let rec readyStep (r: R<'s, 'a>): R<'s, 'a> =
        match r with
        | Next (sold, (Ready, cont)) -> readyStep (cont sold)
        | _ -> r
    
    // Starts a procedure by returning the first result
    let start p s = 
        let firstResult = 
            match p with
            | (Ready, cont) -> cont s
            | (Waiting, cont) -> Next (s, p)
        readyStep firstResult

    // Steps a procedure along by returning the next result.  It is recursive, as internally it 
    // actually steps until the procedure indicates it is waiting for the next input
    let step (s: 's) (r: R<'s, 'a>): R<'s, 'a> =
        
        // Unless we're already done, get the next result and immediately process any "ready" 
        // procedures
        match r with
        | Next (_, (_, cont)) -> cont s |> readyStep 
        | _ -> r

    // Runs a procedure by stepping it for all the events in the sequence.  Note that the 
    // procedure may not yet be complete when the function returns, as it may still be expecting 
    // further events.  In the event that the procedure completes before all the events are 
    // consumed, the function returns early.  The event sequence must be non-empty, or else the 
    // function returns None.
    let run events proc =
        let init = None
        
        let next resultOpt event = 
            let result =
                match resultOpt with
                | None -> Some (start proc event)
                | Some r -> Some (step event r)
            result
        
        let isDone result = 
            match result with
            | Some (Done _) -> true
            | _ -> false

        Seq.scan next init events
        |> Seq.takeUntil isDone
        |> Seq.choose id
        |> Seq.tryLast

    // A procedure that just returns the current state
    let getState: P<'s, 's> = 
        (Ready, fun s -> Done (s, s))

    // A procedure that waits for the next state that matches the predicate.  It does *not* 
    // consider the current state, and always waits for the next one.
    let rec waitForNextState predicate =
        let f s = 
            match predicate s with
            | true -> Done (s, s)
            | false -> Next (s, waitForNextState predicate)
        (Waiting, f)

    // A procedure that returns the state once the given predicate returns true.  If the current 
    // state matches, it completes immediately without waiting for the next state.
    let waitForState predicate =
        let checkCurrentState s =
            if predicate s then Done (s, s)
            else Next (s, waitForNextState predicate)
        
        (Ready, checkCurrentState)
    
    let rec onNextState predicate =
        let f s =
            match predicate s with
            | Some pnext -> Next (s, pnext)
            | None -> Next (s, onNextState predicate)

        (Waiting, f)

    let rec onState predicate =
        let f s =
            match predicate s with
            | Some pnext -> Next (s, pnext)
            | None -> Next (s, onNextState predicate)

        (Ready, f)

    let setState f =
        let cont s = Done (f s, s)
        (Ready, cont)
    
    let proc = PBuilder()

open Proc

let p0 = proc { return 123 }

let p1 (v: 'v) = proc { 
    let! s = getState
    return s, v 
    }

let p2 = proc {
    let! s1 = p0
    let! s2 = p0
    return s1, s2
    }

let p3 = proc {
    let! s = getState
    let! a1, a2 = 
        proc {
            let! a1 = p1 1
            let! a2 = p1 2
            return a1, a2
        }
    let! s2 = waitForState (fun (s: string) -> s.Length > 1)
    let! a3 = p1 3
    let! s3 = getState
    return a1, a2, a3, s, s2, s3
    }

start p0 "initial state"

start (p1 "a") "1"

start p2 "0"

start p3 "0"
start p3 "0" |> step "1"
start p3 "0" |> step "1" |> step "2"
start p3 "0" |> step "1" |> step "2" |> step "3"
start p3 "0" |> step "1" |> step "2" |> step "3" |> step "4"
start p3 "0" |> step "1" |> step "2" |> step "3" |> step "4" |> step "5"
start p3 "0" |> step "1" |> step "2" |> step "3" |> step "4" |> step "5" |> step "60"

start (proc { return! getState }) "asdf" |> step "qwer"

let events = seq { for i in 0 .. 20 do yield i.ToString() }

let result = run events p3

type C<'c> = | List of 'c list

module Cmd =
    let map f (List l) = List (List.map f l)

    let none = List []

    let one c = List [c]

    let add cmd (List l) = List (cmd :: l)

    let batch cmds = List [ for (List l) in cmds do for c in l do yield c ]

type Event =
    | Type1 of int
    | Type2 of string

type M<'s, 'e, 'c> = {
    init: 's * C<'c>
    update: 'e -> 's -> 's * C<'c>
}

type TimerId = int

type ProcedureEvent<'e> =
    | Cancel
    | Timeout of TimerId
    | MachineEvent of 'e

type ProcedureCommand<'c> =
    | StartTimer of TimerId * System.TimeSpan
    | Automation of string * ((string * string) list)
    | MachineCommand of 'c

type TestState<'s, 'e, 'c> = {
    state: 's
    event: ProcedureEvent<'e>
    command: C<ProcedureCommand<'c>>
    machine: M<'s, 'e, 'c>
    nextTimerId: TimerId
    waitTimer: TimerId
    }

type TestProc<'s, 'e, 'c, 'r> = Proc.P<TestState<'s, 'e, 'c>, 'r>

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

let startWaitTimer duration = proc {
    let! s = setState (fun s -> 
        { s with 
            command = 
                Cmd.add (StartTimer duration) s.command 
        })
    return s
    }

let defaultUpdate event testState =
    let (newState, newCmd) = testState.machine.update event testState.state
    { testState with
        state = newState
        command = Cmd.batch [(Cmd.map MachineCommand newCmd); testState.command]
    }

let waitForEvent duration predicate: TestProc<'state, 'event, 'command, 'event option> =
    proc {
        let! timerId = startWaitTimer duration
        let! s = Proc.onNextState (fun s -> 
            match s.event with
            | Timeout timerId -> proc { return None } |> Some
            | MachineEvent e ->
                if predicate e then proc { return Some e } |> Some
                else proc {
                    let! s1 = Proc.setState (defaultUpdate e)
                    return None 
                } |> Some
            | _ -> None)

        return s
    }

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

