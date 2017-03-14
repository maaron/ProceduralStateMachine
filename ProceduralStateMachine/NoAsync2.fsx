﻿// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open ProceduralStateMachine
open System.Collections.Generic
open System

let until p f a =
    let mutable a = a
    while not (p a) do a <- f a
    a

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

// This implementation uses a simple list structure for commands.  Also, the command type is 
// completely distrinct from the event type.  The advantages of this approach are:
//
//   1. Commands are trivial to compare for equality (nice for unit-testing)
//   2. Maybe works better for layered protocols.  Specifically, cases where a lower-layer state 
//      machine is actually the handler for the commands generated by the higher-layer protocol 
//      (as opposed to wrapping them up opaqely).
//
// The dis-advantages are:
//
//   1. More plumbing required for composing state machines, as every state machine must define a 
//      distinct command type.  Also, explicit implementations of the side-effectful functions 
//      must be provided, which will very often be pretty boilerplate (i.e., they just defer to 
//      the wrapped command's side-effect implementation).
//
// An alternative is to use a class similar to Task<T> or IObservable<T>.  These are not normally 
// comparable, but the type "T" is the same as the event type defined by the state machine (or can 
// be easily mapped to it), and thus doesn't required a separately defined command type.
//
// A hybrid approach is to define a Cmd<'e> interface with some Run method (akin to Task<T>, etc),
// but create Cmd<'e> instances using some DataCmd<'d, 'e> type that is comparable in 'd.  Then,
// we can define a Cmd.map implementation that retains the "equality-ness" of the underlying 
// DataCmd<'d, 'e>, but without equality in the function argument.  For example, two DataCmd's 
// with equal mapped data components that are mapped into different values would be considered 
// equal.

type Cmd<'e> = 'e list

module Cmd =
    let map = List.map

    let mappend = List.append

    let none = List.empty

    let one = List.singleton

module Co =
    type Co<'e, 's, 'c, 'a> =
        | Done of 'a
        | Ready of ('s -> Result<'e, 's, 'c, 'a>)
        | Waiting of ('e -> Co<'e, 's, 'c, 'a>)

    and Result<'e, 's, 'c, 'a> = 's * Cmd<'c> * Co<'e, 's, 'c, 'a>
    
    let runReadyResult result =
        let isWaitingOrDone = function 
            | (_, _, Ready k) -> false 
            | _ -> true
        
        let step = function 
            | (s, cmd, Ready k) -> 
                let (s', cmd', step') = k s
                (s', Cmd.mappend cmd cmd', step')
            | _ as r -> r

        until isWaitingOrDone step result

    let rec bind (f: 'a -> Co<'e, 's, 'c, 'b>) (co: Co<'e, 's, 'c, 'a>): Co<'e, 's, 'c, 'b> =
        match co with
        | Done a -> f a
        | Ready k -> Ready (fun s -> 
            let (s', cmd, co') = k s
            runReadyResult (s', cmd, bind f co'))
        | Waiting k -> Waiting (fun e -> bind f (k e))

    let (>>=) p f = bind f p

    let retn a = Ready (fun s -> s, Cmd.none, Done a)

    let map f = bind (fun s -> f s |> retn)

    let (<!>) p f = map f p

    type PBuilder() =
        member x.Bind(p, f) = bind f p
        member x.Return(v: 'a) = retn v
        member x.ReturnFrom(p) = p

    // Starts a procedure by returning the first result
    let start co s = 
        match co with
        | Ready k -> runReadyResult (k s)
        | _ -> (s, Cmd.none, co)

    // Steps a coroutine until it is waiting for the next event.  The returned result contains the 
    // new state, accumulated commands, and a Co<...> representing the remainder of the routine 
    // that can be run once the next event is received.  If the provided coroutine is already done
    // or waiting, the coroutine is returned as-is.
    let step (e: 'e) (r: Result<'e, 's, 'c, 'a>): Result<'e, 's, 'c, 'a> =
        match r with
        | (s', _, Waiting k) -> runReadyResult (s', Cmd.none, k e)
        | _ -> r

    // Applies a sequence of events to a result, stopping early if the procedure completes.
    let stepAll events result =
        let next result event = step event result

        let isDone = function
            | (_, _, Done _) -> true
            | _ -> false

        Seq.scan next result events
        |> Seq.takeUntil isDone

    // Runs a procedure by stepping it for all the events in the sequence.  Note that the 
    // procedure may not yet be complete when the function returns, as it may still be expecting 
    // further events.  In the event that the procedure completes before all the events are 
    // consumed, the function returns early.
    let run init events proc =
        start proc init |> stepAll events

    // A coroutine that just returns the current state
    let get = Ready (fun s -> s, Cmd.none, Done s)

    // A coroutine that waits for and returns the next event
    let wait = Waiting retn
    
    // A coroutine that applies a function to modify the state
    let modify f = Ready (fun s -> f s, Cmd.none, Done ())

    // A coroutine that generates the supplies command
    let tell cmd = Ready (fun s -> s, cmd, Done ())

    let coroutine = PBuilder()

open Co
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

    type M<'e, 's, 'c> = {
        init: 's * Cmd<'c>
        update: 'e -> 's -> 's * Cmd<'c>
    }

    type ProcState<'e, 's, 'c> = 
      { state: 's
        machine: M<'e, 's, 'c>
        nextTimerId: TimerId }

    type Proc<'e, 's, 'c, 'r> = Co<ProcEvent<'e>, ProcState<'e, 's, 'c>, ProcCmd<'c>, 'r>

    let startWaitTimer duration = 
        let update state =
            { state with nextTimerId = state.nextTimerId + 1 }

        coroutine {
            let! state = Co.get
            let! _ = Co.modify update
            do! tell (Cmd.one (StartTimer (state.nextTimerId, duration)))
            return state.nextTimerId
        }

    let stopWaitTimer timerId = 
        tell (Cmd.one (StopTimer timerId))

    let defaultUpdate (event: 'e): Proc<'e, 's, 'c, unit> =
        coroutine {
            let! ps = get
            let (s', cmd) = ps.machine.update event ps.state
            do! modify (fun s -> { s with state = s' })
            do! tell (Cmd.map MachineCommand cmd)
        }

    let handleEventsWithTimeout duration handler =
        let rec tryHandleEvent timerId handler =
            coroutine {
                let! pe = Co.wait

                match pe with
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

    let tryOnEvent duration selectCont: Proc<'state, 'event, 'cmd, 'result option> =
        let handler e = coroutine {
            match selectCont e with
            | Some pnext -> 
                // Predicate matches, run next procedure
                let! result = pnext
                return Some result

            | None -> 
                // Predicate doesn't match, apply default update and keep waiting
                do! defaultUpdate e
                return None
        }

        handleEventsWithTimeout duration handler

    let tryWaitForEvent timeout filter =
        tryOnEvent timeout (filter >> Option.map Co.retn)

    let tryOnState duration selectCont: Proc<'state, 'event, 'command, 'result option> =
        let handler e = coroutine {
            // Apply default update
            do! defaultUpdate e
            let! newState = Co.get
            
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
                machine = machine
                nextTimerId = 0
            }
        start tc procState

    let stepTc event result =
        printf "event: %A\n" event
        Co.step event result

open Proc

type Event =
    | Type1 of int
    | Type2 of string

type Command = string

let processResult result =
    let (s, cmd, co) = result
    match co with
    | Done a ->
        printf "test completed with %A\n" a
        result

    | _ ->
        printf "processing commands %A\n" cmd
        result

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

let fireCommand cmd = Co.tell (Cmd.map MachineCommand (Cmd.one cmd))

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