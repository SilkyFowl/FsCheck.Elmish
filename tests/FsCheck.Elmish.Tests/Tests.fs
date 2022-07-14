module Tests

open System
open Xunit
open FsCheck
open Elmish
open FsCheck.Experimental
open FsCheck.Xunit

module TestModule =
    module Counter =
        type Model = { n: int }

        type Msg =
            | Inc
            | Dec
            | Reset

        let init = { n = 0 }

        let update msg model =
            match msg with
            | Inc -> { model with n = model.n + 1 }
            | Dec -> { model with n = model.n - 1 }
            | Reset -> { model with n = 0 }


    type AsyncOperationStatus<'t> =
        | Started
        | Finished of 't

    type Deferred<'t> =
        | HasNotStartedYet
        | InProgress
        | Resolved of 't

    module RandomNumber =

        type State =
            { RandomNumber: Deferred<Result<double, string>> }

        type Msg = GenerateRandomNumber of AsyncOperationStatus<Result<double, string>>

        type Impl =
            { GenerateRandomNumber: unit -> Async<Result<double, string>> }

        let init () =
            { RandomNumber = HasNotStartedYet }, Cmd.none

        let update impl msg state =
            match msg with
            | GenerateRandomNumber Started when state.RandomNumber = InProgress -> state, Cmd.none

            | GenerateRandomNumber Started ->
                let randomOp: Async<Msg> =
                    async {
                        let! result = impl.GenerateRandomNumber()
                        return Finished result |> GenerateRandomNumber
                    }

                { state with RandomNumber = InProgress }, Cmd.OfAsync.result randomOp

            | GenerateRandomNumber (Finished result) ->
                let nextState = { state with RandomNumber = Resolved result }
                nextState, Cmd.none

        let baseImpl =
            let rnd = System.Random()

            { GenerateRandomNumber =
                fun () ->
                    async {
                        do! Async.Sleep 1000
                        let random = rnd.NextDouble()

                        if random > 0.5 then
                            return Ok 0.0
                        else
                            let errorMsg = sprintf "Failed! Random number %f was <= 0.5" random
                            return Error errorMsg
                    } }

    module App =
        type Page =
            | Counter
            | RandomNumber

        type State =
            { Counter: Counter.Model
              RandomNumber: RandomNumber.State
              CurrentPage: Page }

        type Msg =
            | CounterMsg of Counter.Msg
            | RandomNumberMsg of RandomNumber.Msg
            | SwitchPage of Page

        type Impl = { RandomNumberImpl: RandomNumber.Impl }

        let init () =
            let randomNumberState, randomNumberCmd = RandomNumber.init ()

            let initialState =
                { Counter = Counter.init
                  RandomNumber = randomNumberState
                  CurrentPage = Counter }

            let initialCmd = Cmd.batch [ Cmd.map RandomNumberMsg randomNumberCmd ]
            initialState, initialCmd

        let update impl =
            let randomNumberUpdate = RandomNumber.update impl.RandomNumberImpl

            fun (msg: Msg) (state: State) ->
                match msg with
                | CounterMsg counterMsg ->
                    let updatedCounter = Counter.update counterMsg state.Counter
                    { state with Counter = updatedCounter }, Cmd.none

                | RandomNumberMsg randomNumberMsg ->
                    let updatedRandomNumber, randomNumberCmd =
                        randomNumberUpdate randomNumberMsg state.RandomNumber

                    let appCmd = Cmd.map RandomNumberMsg randomNumberCmd

                    { state with RandomNumber = updatedRandomNumber }, appCmd

                | SwitchPage page -> { state with CurrentPage = page }, Cmd.none


module CounterStateMachine =
    open TestModule
    open Counter

    let createSpec<'Model, 'Msg> =
        ElmishStateMachine.createFuncSimple
            (fun (modelMapper: 'Model -> Model) (toParentMsg: Msg -> 'Msg) totMsg currentModel ->
                [ { new ElmishOperationSimple<_, _, _>() with
                      override __.ToString() = "inc"
                      member _.Run m = { m with n = m.n + 1 }

                      member _.Check(c, m) =
                          toParentMsg Inc |> c.Update

                          m = modelMapper c.State
                          |@ sprintf "Inc: model = %A, actual = %A" m c.State }

                  { new ElmishOperationSimple<_, _, _>() with
                      override __.ToString() = "dec"
                      member _.Run m = { m with n = m.n - 1 }

                      member _.Check(c, m) =
                          toParentMsg Dec |> c.Update

                          m = modelMapper c.State
                          |@ sprintf "Dec: model = %A, actual = %A" m c.State } ]
                |> Gen.elements)


    let basic = createSpec (Gen.constant init) id id id update

    [<Property>]
    let BasicStateMachine () = StateMachine.toProperty basic

module RandomNumberStateMachine =
    open TestModule
    open RandomNumber

    let mockImpl = { GenerateRandomNumber = fun () -> async { return Ok 0.0 } }

    let createSpec<'ParentModel, 'ParentMsg> =


        ElmishStateMachine.createFunc
            (fun (modelMapper: 'ParentModel -> State) (toParentMsg: Msg -> 'ParentMsg) toMsg (currentState, currentCmd) ->

                let startMsgOperation =
                    gen {
                        let startedMsg = GenerateRandomNumber Started

                        return
                            { new ElmishOperation<_, _, _>() with
                                override __.ToString() = $"%A{startedMsg}"

                                member _.Run m =
                                    let state, cmd = m

                                    match state with
                                    | { RandomNumber = InProgress } ->
                                        { state with RandomNumber = InProgress }, Cmd.none
                                    | { RandomNumber = _ } ->
                                        let cmd =
                                            async {
                                                let! result = mockImpl.GenerateRandomNumber()
                                                return result |> Finished |> GenerateRandomNumber
                                            }
                                            |> Cmd.OfAsync.result

                                        { state with RandomNumber = InProgress }, cmd

                                member _.Check(c, (mState, mCmds)) =
                                    startedMsg |> toParentMsg |> c.Update
                                    let state = c.State
                                    let cmds = c.Cmds |> Cmd.map toMsg

                                    modelMapper state = mState
                                    |@ $"%A{startedMsg}: model = %A{mState}, actual = %A{c.State}"
                                    .&. Cmd.assertMsg mCmds cmds }
                    }

                let finishedMsgOperation =
                    gen {
                        let! flg = Arb.generate
                        let! v = Arb.Default.NormalFloat() |> Arb.toGen
                        let! msg = Arb.Default.UnicodeString() |> Arb.toGen

                        let result =
                            if flg then
                                Ok(float v)
                            else
                                Error(string msg)

                        let msg = GenerateRandomNumber(Finished result)

                        return
                            { new ElmishOperation<_, _, _>() with
                                override __.ToString() = $"%A{msg}"

                                member _.Run m =
                                    let state, cmd = m
                                    { state with RandomNumber = Resolved result }, Cmd.none

                                member _.Check(c, (mState, mCmds)) =
                                    msg |> toParentMsg |> c.Update
                                    let state = c.State
                                    let cmds = c.Cmds |> Cmd.map toMsg


                                    modelMapper state = mState
                                    |@ $"%A{msg}: model = %A{mState}, actual = %A{c.State}"
                                    .&. Cmd.assertMsg mCmds cmds }
                    }

                Gen.oneof [ startMsgOperation
                            finishedMsgOperation ])
        |> ElmishStateMachine.withUpdate (fun update -> update mockImpl)

    let basicSpec =
        let initGen = init () |> Gen.constant
        createSpec initGen id id id update

    [<Property>]
    let BasicStateMachine () = StateMachine.toProperty basicSpec


module AppStateMachine =
    open TestModule.App

    [<Property>]
    let ``App.Counter StateMachine`` () =
        let initGen =
            let state, _ = init ()
            state |> Gen.constant

        let modelMapper state = state.Counter
        let toParentMsg = CounterMsg

        let toChildMsg =
            function
            | CounterMsg msg -> msg
            | invalid -> invalidArg "invalid" "invalid msg"

        let impl = { RandomNumberImpl = RandomNumberStateMachine.mockImpl }

        let update msg state =
            let state, _ = update impl msg state
            state

        CounterStateMachine.createSpec initGen modelMapper toParentMsg toChildMsg update
        |> StateMachine.toProperty

    [<Property>]
    let ``App.RandomNumber StateMachine`` () =
        let initGen = init () |> Gen.constant
        let modelMapper state = state.RandomNumber
        let toParentMsg = RandomNumberMsg

        let toChildMsg =
            function
            | RandomNumberMsg msg -> msg
            | invalid -> invalidArg "invalid" "invalid msg"

        let update impl = update { RandomNumberImpl = impl }

        RandomNumberStateMachine.createSpec initGen modelMapper toParentMsg toChildMsg update
        |> StateMachine.toProperty


    let createSpec<'ParentModel, 'ParentMsg> =
        ElmishStateMachine.createFunc
            (fun (modelMapper: 'ParentModel -> State) (toParentMsg: Msg -> 'ParentMsg) toMsg (currentState, currentCmd) ->
                gen {
                    let! page = Arb.generate
                    let msg = SwitchPage page

                    return
                        { new ElmishOperation<_, _, _>() with
                            override __.ToString() = $"%A{msg}"

                            member _.Run m =
                                let state, cmd = m
                                { state with CurrentPage = page }, Cmd.none

                            member _.Check(c, (mState, mCmds)) =
                                msg |> toParentMsg |> c.Update
                                let state = c.State
                                let cmds = c.Cmds |> Cmd.map toMsg

                                modelMapper state = mState
                                |@ $"%A{msg}: model = %A{mState}, actual = %A{c.State}"
                                .&. Cmd.assertMsg mCmds cmds }
                })

    [<Property>]
    let ``App StateMachine`` () =
        let initGen = init () |> Gen.constant
        let update = update { RandomNumberImpl = RandomNumberStateMachine.mockImpl }

        createSpec initGen id id id update
        |> StateMachine.toProperty
