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

    // A procedure that waits for and returns the next state
    let getNextState: P<'s, 's> =
        (Waiting, fun s -> Done (s, s))

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
    let! s3 = getNextState
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
start p3 "0" |> step "1" |> step "2" |> step "3" |> step "4" |> step "50" |> step "60"

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
    | TimerExpired of TimerId
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
    }

type TestProc<'s, 'e, 'c, 'r> = Proc.P<TestState<'s, 'e, 'c>, 'r>

let startWaitTimer duration = 
    printf "startWaitTimer\n"
    let update state =
        { state with 
            nextTimerId = state.nextTimerId + 1
            command = 
                Cmd.add (StartTimer (state.nextTimerId, duration)) state.command 
        }

    proc {
        let! state = Proc.setState update
        return state.nextTimerId
    }

let defaultUpdate event testState =
    let (newState, newCmd) = testState.machine.update event testState.state
    { testState with
        state = newState
        command = Cmd.batch [(Cmd.map MachineCommand newCmd); testState.command]
    }



let tryWaitForEvent duration predicate: TestProc<'state, 'event, 'command, 'result option> =
    let rec processEvent predicate timerId = 
        proc {
            let! state = Proc.getNextState

            match state.event with
            | TimerExpired id when id = timerId -> 
                return None

            // Predicate matches, give event to proc
            | MachineEvent e ->
                match predicate e with
                | Some r -> return Some r
                | None -> 
                    let! s1 = Proc.setState (defaultUpdate e)
                    return! processEvent predicate timerId

            // Non-matching timer- keep waiting
            | _ -> return! processEvent predicate timerId
        }

    proc {
        printf "starting timer\n"
        let! timerId = startWaitTimer duration
        printf "timer started %A\n" timerId
        return! processEvent predicate timerId
    }

let tryOnEvent duration selectCont: TestProc<'state, 'event, 'command, 'result option> =
    let rec processEvent selectCont timerId = 
        proc {
            let! state = Proc.getNextState

            match state.event with
            | TimerExpired id when id = timerId -> 
                return None

            // Predicate matches, give event to proc
            | MachineEvent e ->
                match selectCont e with
                | Some pnext -> 
                    let! result = pnext
                    return Some result
                | None -> return! processEvent selectCont timerId

            // Non-matching timer- keep waiting
            | _ -> return! processEvent selectCont timerId
        }

    proc {
        let! timerId = startWaitTimer duration
        return! processEvent selectCont timerId
    }

let startTc machine tc =
    let procState =
        { 
            state = fst machine.init
            event = MachineEvent (Type1 123)
            command = Cmd.map MachineCommand (snd machine.init)
            machine = machine
            nextTimerId = 0
        }
    start tc procState

let processResult result =
    match result with
    | Done (s, a) ->
        printf "test completed with %A\n" a
        Done (s, a)

    | Next (s, pnext) ->
        printf "processing commands %A\n" s.command
        Next ({ s with command = Cmd.none }, pnext)

let stepTc event result =
    match result with
    | Done (s, a) -> Done ({ s with event = event }, a)
    | Next (s, pnext) -> Proc.step { s with event = event } result

type Command = string

let eventType1 event =
    match event with | Type1 i -> Some i | _ -> None

let machine = {
    init = 0, Cmd.one "initialize"
    update = fun (e: Event) s -> 
        s + 1, Cmd.one "command "
    }

let tc1 = proc {
    let! event = tryWaitForEvent (System.TimeSpan.FromSeconds 1.0) eventType1
    return event
    }

startTc machine tc1 |> processResult 
|> stepTc (MachineEvent (Type2 "123")) |> processResult 
|> stepTc (MachineEvent (Type1 2345)) |> processResult

let onEventType2 timeout proc =
    tryOnEvent timeout (function | Type2 v -> Some (proc v) | _ -> None)

let event2Handler (event: string) = proc { return event.Length }

let tc2 = proc {
    let timeout = TimeSpan.FromSeconds 1.0

    let! e1 = tryWaitForEvent timeout eventType1

    let! e2 = onEventType2 timeout event2Handler

    let! e3 = 
        tryOnEvent timeout (function 
            | Type1 e1 -> Some (proc { return "Got a type1 event" })
            //| Type2 e2 -> Some (Proc.retn "Got a type2 event")
            | _ -> None)

    return e1, e2, e3
    }

start tc2 
    { 
        state = fst machine.init
        event = MachineEvent (Type1 123)
        command = Cmd.none
        machine = machine
        nextTimerId = 0
    }

