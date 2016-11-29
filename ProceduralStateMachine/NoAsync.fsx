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

    type P<'s, 'a> = P of Wait * ('s -> R<'s, 'a>)

    and R<'s, 'a> = 
        | Done of 's * 'a
        | Next of 's * P<'s, 'a>

    let rec bind f p =
        let (P (wait, cont)) = p
        let step state =
            match cont state with
            | Done (s, a) -> Next (s, f a)
            | Next (s, pnext) -> Next (s, bind f pnext)
        P (wait, step)

    let (>>=) p f = bind f p

    let returnProc v = P (Ready, fun s -> Done (s, v))

    let map f = bind (fun s -> f s |> returnProc)

    let (<!>) p f = map f p

    type PBuilder() =
        member x.Bind(p: P<'s, 'a>, f: 'a -> P<'s, 'a2>): P<'s, 'a2> = bind f p

        member x.Return(v: 'a): P<'s, 'a> = returnProc v

        member x.ReturnFrom(p) = p

    // This function recursively applies the current state until the procedure is either done 
    // or waiting for the next state
    let rec readyStep (r: R<'s, 'a>): R<'s, 'a> =
        match r with
        | Next (sold, P (Ready, cont)) -> readyStep (cont sold)
        | _ -> r
    
    // Starts a procedure by returning the first result
    let start p s = 
        let firstResult = 
            match p with
            | P (Ready, cont) -> cont s
            | P (Waiting, cont) -> Next (s, p)
        readyStep firstResult

    // Steps a procedure along by returning the next result.  It is recursive, as internally it 
    // actually steps until the procedure indicates it is waiting for the next input
    let step (s: 's) (r: R<'s, 'a>): R<'s, 'a> =
        // Unless we're already done, get the next result and immediately process any "ready" 
        // procedures
        match r with
        | Next (_, P (_, cont)) -> cont s |> readyStep 
        | _ -> r

    // Applies a sequence of events to a result, stopping early if the procedure completes.
    let stepAll events result =
        let next result event = step event result

        let isDone = function
            | Done _ -> true
            | _ -> false

        Seq.scan next result events
        |> Seq.takeUntil isDone

    // Runs a procedure by stepping it for all the events in the sequence.  Note that the 
    // procedure may not yet be complete when the function returns, as it may still be expecting 
    // further events.  In the event that the procedure completes before all the events are 
    // consumed, the function returns early.
    let run event events proc =
        start proc event |> stepAll events

    let getState2 f =
        P (Ready, fun s -> Done (s, f s))

    // A procedure that just returns the current state
    let getState: P<'s, 's> = 
        P (Ready, fun s -> Done (s, s))

    // A procedure that waits for and returns the next state
    let getNextState: P<'s, 's> =
        P (Waiting, fun s -> Done (s, s))
    
    // A procedure that applies a function to modify the state
    let setState f =
        let cont s = let snew = f s in Done (snew, snew)
        P (Ready, cont)
    
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
    let! s2 = getNextState
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

let (event, events) = "0", [ for i in 1 .. 20 do yield i.ToString() ]

let result = run event events p3

type C<'c> = | List of 'c list

module Cmd =
    let map f (List l) = List (List.map f l)

    let none = List []

    let one c = List [c]

    let add cmd (List l) = List (cmd :: l)

    let batch cmds = List [ for (List l) in cmds do for c in l do yield c ]

type TimerId = int

type ProcedureEvent<'e> =
    | Start
    | TimerExpired of TimerId
    | MachineEvent of 'e

type ProcedureCommand<'c> =
    | StartTimer of TimerId * System.TimeSpan
    | StopTimer of TimerId
    | Automation of string * ((string * string) list)
    | MachineCommand of 'c

type M<'s, 'e, 'c> = {
    init: 's * C<'c>
    update: 'e -> 's -> 's * C<'c>
}

type TestState<'s, 'e, 'c> = {
    state: 's
    event: ProcedureEvent<'e>
    command: C<ProcedureCommand<'c>>
    machine: M<'s, 'e, 'c>
    nextTimerId: TimerId
    }

type TestProc<'s, 'e, 'c, 'r> = Proc.P<TestState<'s, 'e, 'c>, 'r>

let startWaitTimer duration = 
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

let stopWaitTimer timerId = 
    let update state =
        { state with 
            command = 
                Cmd.add (StopTimer timerId) state.command 
        }

    proc {
        let! state = Proc.setState update
        return ()
    }

let defaultUpdate event testState =
    let (newState, newCmd) = testState.machine.update event testState.state
    { testState with
        state = newState
        command = Cmd.batch [(Cmd.map MachineCommand newCmd); testState.command]
    }

let handleEventsWithTimeout duration handler =
    let rec tryHandleEvent timerId handler =
        proc {
            let! state = Proc.getNextState

            match state.event with
            | TimerExpired id when id = timerId -> 
                return None

            | MachineEvent e -> 
                // Give the event to the handler, who'll tell us whether to keep waiting or not
                let! o = handler e
                match o with
                | Some r -> 
                    // Got a result, stop waiting and return it
                    return Some r

                | None -> 
                    // No result, keep waiting
                    return! tryHandleEvent timerId handler

            // Non-matching timer- keep waiting
            | _ -> return! tryHandleEvent timerId handler
        }

    proc {
        let! timerId = startWaitTimer duration
        let! result = tryHandleEvent timerId handler

        match result with
        | Some value -> 
            // If we didn't timeout, send a command to stop the timer
            do! stopWaitTimer timerId
            return result
        
        | _ -> 
            return result
    }

let tryOnEvent duration selectCont: TestProc<'state, 'event, 'command, 'result option> =
    let handler e = proc {
        match selectCont e with
        | Some pnext -> 
            // Predicate matches, run next procedure
            let! result = pnext
            return Some result

        | None -> 
            // Predicate doesn't match, apply default update and keep waiting
            let! s1 = Proc.setState (defaultUpdate e)
            return None
    }

    handleEventsWithTimeout duration handler

let tryWaitForEvent duration predicate =
    tryOnEvent duration (predicate >> Option.map Proc.returnProc)

let tryOnState duration selectCont: TestProc<'state, 'event, 'command, 'result option> =
    let handler e = proc {
        // Apply default update
        let! newState = Proc.setState (defaultUpdate e)
            
        match selectCont newState.state with
        | Some cont -> 
            // Predicate matches, return result to caller
            let! result = cont
            return Some result

        | None -> 
            // Predicate doesn't match, keep waiting
            return None
    }

    handleEventsWithTimeout duration handler

let tryWaitForState duration predicate =
    tryOnState duration (predicate >> Option.map Proc.returnProc)

type Event =
    | Type1 of int
    | Type2 of string

let startTc machine tc =
    let procState =
        { 
            state = fst machine.init
            event = Start
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
    printf "event: %A\n" event
    match result with
    | Done (s, a) -> Done ({ s with event = event }, a)
    | Next (s, pnext) -> Proc.step { s with event = event } result

type Command = string

let eventType1 event =
    match event with 
    | Type1 i -> Some i 
    | _ -> None

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

let fireCommand cmd = proc { 
    let! _ = setState (fun s -> { s with command = Cmd.add (MachineCommand cmd) s.command })
    return ()
    }

let tc2 = proc {
    let timeout = TimeSpan.FromSeconds 1.0

    let! e1 = tryWaitForEvent timeout eventType1

    do! fireCommand <| string e1

    let! e2 = onEventType2 timeout event2Handler

    let! e3 = 
        tryOnEvent timeout (function 
            | Type1 e1 -> Some <| proc { return sprintf "Got a type1 event %A" e1 }
            | Type2 e2 -> Some <| proc { return sprintf "Got a type2 event %A" e2 }
            | _ -> None)

    do! fireCommand "do something"

    let! eventOrTimeout = 
        tryWaitForEvent timeout eventType1 >>= function 
            | Some e4 -> returnProc (sprintf "event %A" e4)
            | None -> returnProc "timeout"

    let! eventOrTimeout2 = 
        tryWaitForEvent timeout eventType1 <!> function 
            | Some e4 -> sprintf "event %A" e4
            | None -> "timeout"

    return e1, e2, e3, eventOrTimeout, eventOrTimeout2
    }

startTc machine tc2 |> processResult 
|> stepTc (MachineEvent (Type1 2345)) |> processResult
|> stepTc (MachineEvent (Type2 "12345")) |> processResult 
|> stepTc (MachineEvent (Type2 "123 #2")) |> processResult 
|> stepTc (TimerExpired 4) |> processResult 
|> stepTc (MachineEvent (Type1 111)) |> processResult
