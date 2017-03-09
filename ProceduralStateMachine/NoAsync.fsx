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

// I think there might be a slightly better design possible here, although I doubt it changes the 
// overall interface.  The Co type is currently parameterized over a state ('s) type that actually
// will contain the events and commands when used in practice.  But, the Wait enum is solely there
// for indicating whether to wait for an event.  It may be better to just parameterize Co over the 
// event type explicitly (and maybe the command, too?).  Maybe something like this:
//
// type Co<'e, 's, 'a> =
//   | Wait of ('e -> 's -> R<'s, 'a>)
//   | Ready of ('s -> R<'s, 'a>)
//
// If we let commands be parameterized over the event type (useful for making machines 
// composable), we might change the R type to this:
//
// type R<'e, 's, 'a> =
//   | Done of 's, Cmd<'e>, 'a
//   | Next of 's, Cmd<'a>, Co<'e, 's, 'a>
//
// This also implies moving the Cmd-aggregating code into the Co bind function, very much like a 
// writer monad.  I think the benefit here might be more type-safe use of the ultimate state type 
// used:
//
// type ProcedureState<'s, 'e, 'c> = 
//   { state: 's
//     event: ProcEvent<'e>
//     command: Cmd<ProcCmd<'c>>
//     machine: M<'s, 'e, 'c>
//     nextTimerId: TimerId }
//
// We can pull the event and command fields out of this, instead of exposing them for accidental 
// update/use by the state-modifying functions.

module Co =
    type Wait = Waiting | Ready

    type Co<'s, 'a> = Co of Wait * ('s -> R<'s, 'a>)

    and R<'s, 'a> = 
        | Done of 's * 'a
        | Next of 's * Co<'s, 'a>

    let rec bind f p =
        let (Co (wait, cont)) = p
        let step state =
            match cont state with
            | Done (s, a) -> Next (s, f a)
            | Next (s, pnext) -> Next (s, bind f pnext)
        Co (wait, step)

    let (>>=) p f = bind f p

    let retn v = Co (Ready, fun s -> Done (s, v))

    let map f = bind (fun s -> f s |> retn)

    let (<!>) p f = map f p

    type PBuilder() =
        member x.Bind(p: Co<'s, 'a>, f: 'a -> Co<'s, 'a2>): Co<'s, 'a2> = bind f p
        member x.Return(v: 'a): Co<'s, 'a> = retn v
        member x.ReturnFrom(p) = p

    // This function recursively applies the current state until the procedure is either done 
    // or waiting for the next state
    let rec readyStep (r: R<'s, 'a>): R<'s, 'a> =
        match r with
        | Next (sold, Co (Ready, cont)) -> readyStep (cont sold)
        | _ -> r
    
    // Starts a procedure by returning the first result
    let start p s = 
        let firstResult = 
            match p with
            | Co (Ready, cont) -> cont s
            | Co (Waiting, cont) -> Next (s, p)
        readyStep firstResult

    // Steps a procedure along by returning the next result.  It is recursive, as internally it 
    // actually steps until the procedure indicates it is waiting for the next input
    let step (s: 's) (r: R<'s, 'a>): R<'s, 'a> =
        // Unless we're already done, get the next result and immediately process any "ready" 
        // procedures
        match r with
        | Next (_, Co (_, cont)) -> cont s |> readyStep 
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

    // A procedure that just returns the current state
    let getState: Co<'s, 's> = 
        Co (Ready, fun s -> Done (s, s))

    // A procedure that waits for and returns the next state
    let getNextState: Co<'s, 's> =
        Co (Waiting, fun s -> Done (s, s))
    
    // A procedure that applies a function to modify the state
    let setState f =
        let cont s = let snew = f s in Done (snew, snew)
        Co (Ready, cont)
    
    let coroutine = PBuilder()

open Co

let p0 = coroutine { return 123 }

let p1 (v: 'v) = coroutine { 
    let! s = getState
    return s, v 
    }

let p2 = coroutine {
    let! s1 = p0
    let! s2 = p0
    return s1, s2
    }

let p3 = coroutine {
    let! s = getState
    let! a1, a2 = 
        coroutine {
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

start (coroutine { return! getState }) "asdf" |> step "qwer"

let (event, events) = "0", [ for i in 1 .. 20 do yield i.ToString() ]

let result = run event events p3

module Cmd =
    type Cmd<'c> = | List of 'c list

    let map f (List l) = List (List.map f l)

    let none = List []

    let one c = List [c]

    let add cmd (List l) = List (cmd :: l)

    let batch cmds = List [ for (List l) in cmds do for c in l do yield c ]

open Cmd

module Proc =

    type TimerId = int

    type ProcEvent<'e> =
        | Start
        | TimerExpired of TimerId
        | MachineEvent of 'e

    type ProcCmd<'c> =
        | StartTimer of TimerId * System.TimeSpan
        | StopTimer of TimerId
        | Automation of string * ((string * string) list)
        | MachineCommand of 'c

    type M<'s, 'e, 'c> = {
        init: 's * Cmd<'c>
        update: 'e -> 's -> 's * Cmd<'c>
    }

    type ProcState<'s, 'e, 'c> = {
        state: 's
        event: ProcEvent<'e>
        command: Cmd<ProcCmd<'c>>
        machine: M<'s, 'e, 'c>
        nextTimerId: TimerId
        }

    type Proc<'s, 'e, 'c, 'r> = Co<ProcState<'s, 'e, 'c>, 'r>

    let startWaitTimer duration = 
        let update state =
            { state with 
                nextTimerId = state.nextTimerId + 1
                command = 
                    Cmd.add (StartTimer (state.nextTimerId, duration)) state.command 
            }

        coroutine {
            let! state = Co.getState
            let! _ = Co.setState update
            return state.nextTimerId
        }

    let stopWaitTimer timerId = 
        let update state =
            { state with 
                command = 
                    Cmd.add (StopTimer timerId) state.command 
            }

        coroutine {
            let! state = Co.setState update
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
            coroutine {
                let! state = Co.getNextState

                match state.event with
                | TimerExpired id when id = timerId -> 
                    // Timer expired, stop waiting
                    return None

                | MachineEvent e -> 
                    // Give the event to the handler, who'll tell us whether to keep waiting or not
                    let! handlerResult = handler e
                    match handlerResult with
                    | Some r -> 
                        // Got a result, stop waiting and return it
                        return Some r

                    | None -> 
                        // No result, keep waiting
                        return! tryHandleEvent timerId handler

                | _ -> 
                    // Non-matching timer- keep waiting
                    return! tryHandleEvent timerId handler
            }

        coroutine {
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

    let tryOnEvent duration selectCont: Proc<'state, 'event, 'command, 'result option> =
        let handler e = coroutine {
            match selectCont e with
            | Some pnext -> 
                // Predicate matches, run next procedure
                let! result = pnext
                return Some result

            | None -> 
                // Predicate doesn't match, apply default update and keep waiting
                let! s1 = Co.setState (defaultUpdate e)
                return None
        }

        handleEventsWithTimeout duration handler

    let tryWaitForEvent timeout filter =
        tryOnEvent timeout (filter >> Option.map Co.retn)

    let tryOnState duration selectCont: Proc<'state, 'event, 'command, 'result option> =
        let handler e = coroutine {
            // Apply default update
            let! newState = Co.setState (defaultUpdate e)
            
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
        tryOnState duration (predicate >> Option.map Co.retn)

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

    let stepTc event result =
        printf "event: %A\n" event
        match result with
        | Done (s, a) -> Done ({ s with event = event }, a)
        | Next (s, pnext) -> Co.step { s with event = event } result

open Proc

type Event =
    | Type1 of int
    | Type2 of string

type Command = string

let processResult result =
    match result with
    | Done (s, a) ->
        printf "test completed with %A\n" a
        Done (s, a)

    | Next (s, pnext) ->
        printf "processing commands %A\n" s.command
        Next ({ s with command = Cmd.none }, pnext)

let eventType1 event =
    match event with 
    | Type1 i -> Some i 
    | _ -> None

let machine = {
    init = 0, Cmd.one "initialize"
    update = fun (e: Event) s -> 
        s + 1, Cmd.one "command "
    }

let tc1 = coroutine {
    let! event = tryWaitForEvent (System.TimeSpan.FromSeconds 1.0) eventType1
    return event
    }

startTc machine tc1 |> processResult 
|> stepTc (MachineEvent (Type2 "123")) |> processResult 
|> stepTc (MachineEvent (Type1 2345)) |> processResult

let onEventType2 timeout proc =
    tryOnEvent timeout (function | Type2 v -> Some (proc v) | _ -> None)

let event2Handler (event: string) = coroutine { return event.Length }

let fireCommand cmd = coroutine { 
    let! _ = setState (fun s -> { s with command = Cmd.add (MachineCommand cmd) s.command })
    return ()
    }

type TestResult<'t> =
    | Pass of 't
    | Fail of string

let tc2 = coroutine {
    let timeout = TimeSpan.FromSeconds 1.0

    let! e1 = tryWaitForEvent timeout eventType1

    if e1.IsNone then return Fail "timed out" else

    do! fireCommand <| string e1

    let! e2 = onEventType2 timeout event2Handler

    let! e3 = 
        tryOnEvent timeout <| function 
            | Type1 e1 -> Some <| coroutine { return sprintf "Got a type1 event %A" e1 }
            | Type2 e2 -> Some <| coroutine { return sprintf "Got a type2 event %A" e2 }
            | _ -> None

    do! fireCommand "do something"

    let! eventOrTimeout = 
        tryWaitForEvent timeout eventType1 >>= function 
            | Some e4 -> retn (sprintf "event %A" e4)
            | None -> retn "timeout  (bind)"

    let! eventOrTimeout2 = 
        tryWaitForEvent timeout eventType1 <!> function 
            | Some e4 -> sprintf "event %A" e4
            | None -> "timeout (map)"

    return Pass (e1, e2, e3, eventOrTimeout, eventOrTimeout2)
    }

startTc machine tc2 |> processResult 
|> stepTc (MachineEvent (Type1 2345)) |> processResult
|> stepTc (MachineEvent (Type2 "12345")) |> processResult 
|> stepTc (MachineEvent (Type2 "123 #2")) |> processResult 
|> stepTc (TimerExpired 3) |> processResult 
|> stepTc (MachineEvent (Type1 111)) |> processResult

module Test =
    type Test<'s, 'e, 'c, 'a> = Proc<'s, 'e, 'c, TestResult<'a>>
    
    let bind f t = coroutine {
        let! result = t
        match result with
        | Pass value -> return! f value
        | Fail reason -> return Fail reason
    }

    let map f = bind (fun s -> f s |> Co.retn)

    let retn v = Co.retn (Pass v)

    let liftProc = Co.map Pass

    type TestBuilder() =
        member x.Bind(t, f) = bind f t
        member x.Return(v) = retn v

    let test = TestBuilder()

    let onEvent timeout next = 
        tryOnEvent timeout next <!> function
            | Some r -> Pass r
            | None -> Fail "Timed out waiting for event"

    let waitForEvent timeout predicate =
        tryWaitForEvent timeout predicate <!> function
            | Some r -> Pass r
            | None -> Fail "Timed out waiting for event"

    let mapFail message =
        function
        | Some r -> Pass r
        | None -> Fail message

    let waitForEvent1 timeout =
        tryWaitForEvent timeout (function | Type1 e -> Some e | _ -> None) 
        <!> mapFail "Timed out waiting for event 1"

    let waitForEvent2 timeout =
        tryWaitForEvent timeout (function | Type2 e -> Some e | _ -> None) 
        <!> mapFail "Timed out waiting for event 2"

open Test

let testCase1 = test {
    let timeout = TimeSpan.FromSeconds 1.0
    
    let! e1 = waitForEvent1 timeout
    
    let! e2 = waitForEvent timeout eventType1

    let! e3 = tryWaitForEvent timeout eventType1 |> liftProc

    return "yeah! I passed!", e1, e2, e3
}

startTc machine testCase1 |> processResult
|> stepTc (MachineEvent (Type1 42)) |> processResult
|> stepTc (MachineEvent (Type1 43)) |> processResult
|> stepTc (MachineEvent (Type1 44)) |> processResult

startTc machine testCase1 |> processResult
|> stepTc (TimerExpired 0) |> processResult