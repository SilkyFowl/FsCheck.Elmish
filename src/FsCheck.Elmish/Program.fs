module FsCheck.Elmish

open FsCheck
open FsCheck.Experimental
open Elmish


module Cmd =

    let captureMsg (cmd: Cmd<'Msg>) =
        let mutable counter = List.length cmd
        let arr = ResizeArray()

        for call in cmd do
            fun msg ->
                arr.Add msg
                counter <- counter - 1
            |> call

        while counter > 0 do
            ()

        Seq.readonly arr

    let captureMsgAsync (cmd: Cmd<'Msg>) = async { return captureMsg cmd }

    let private printerrormsg expect actual = sprintf "\n\nexpected cmdMsg = %A\n\nactual cmdMsg = %A\n" expect actual
    let assertMsgAsync expect actual =
        async {
            let! expectMsg = captureMsgAsync expect
            let! actualMsg = captureMsgAsync actual

            return
                (Seq.length expectMsg = Seq.length actualMsg
                 && actualMsg |> Seq.forall2 (=) expectMsg)
                |@ printerrormsg expectMsg actualMsg
        }


    let assertMsg expect actual =
        let expectMsg = captureMsg expect
        let actualMsg = captureMsg actual

        (Seq.length expectMsg = Seq.length actualMsg
         && actualMsg |> Seq.forall2 (=) expectMsg)
        |@ printerrormsg expectMsg actualMsg


type ElmishContainerSimple<'Model, 'Msg>(init, update) =
    let lockObj = obj ()
    let mutable state: 'Model = init
    member _.State = state

    member _.Update(msg: 'Msg) =
        lock lockObj (fun _ -> state <- update msg state)

[<StructuredFormatDisplay("{DisplayText}")>]
type ElmishContainer<'Model, 'Msg>(init, update) =
    let lockObj = obj ()
    let mutable state = init
    member _.State: 'Model = fst state
    member _.Cmds: Cmd<'Msg> = snd state

    member _.Update(msg: 'Msg) =
        lock lockObj (fun _ -> state <- fst state |> update msg)
    
    override this.ToString() =
        $"%A{this.State} %A{Cmd.captureMsg this.Cmds}"

type ElmishOperationSimple<'ParentModel, 'ParentMsg, 'Model> =
    Operation<ElmishContainerSimple<'ParentModel, 'ParentMsg>, 'Model>

type ElmishOperation<'ParentModel, 'ParentMsg, 'Model> = Operation<ElmishContainer<'ParentModel, 'ParentMsg>, 'Model>

module ElmishStateMachine =
    let createFuncSimple
        (operations: ('ParentModel -> 'Model)
                         -> ('Msg -> 'ParentMsg)
                         -> ('ParentMsg -> 'Msg)
                         -> 'Model
                         -> (ElmishOperationSimple<'ParentModel, 'ParentMsg, 'Model>) Gen)
        =
        fun parentInit modelMapper toParentMsg totMsg parentUpdate ->
            let create init =
                let container = ElmishContainerSimple(init, parentUpdate)

                { new Setup<ElmishContainerSimple<'ParentModel, 'ParentMsg>, 'Model>() with
                    member _.Actual() = container
                    member _.Model() = container.State |> modelMapper }

            { new Machine<ElmishContainerSimple<'ParentModel, 'ParentMsg>, 'Model>() with
                member _.Setup = parentInit |> Gen.map create |> Arb.fromGen

                member __.Next model =
                    operations modelMapper toParentMsg totMsg model }

    let createFunc
        (operations: ('ParentModel -> 'Model)
                         -> ('Msg -> 'ParentMsg)
                         -> ('ParentMsg -> 'Msg)
                         -> ('Model * Cmd<'Msg>)
                         -> (ElmishOperation<'ParentModel, 'ParentMsg, 'Model * Cmd<'Msg>>) Gen)
        =
        fun parentInit modelMapper toParentMsg totMsg parentUpdate ->
            let create init =
                let container = ElmishContainer(init, parentUpdate)

                { new Setup<ElmishContainer<'ParentModel, 'ParentMsg>, 'Model * Cmd<'Msg>>() with
                    member _.Actual() = container

                    member _.Model() =
                        container.State |> modelMapper, container.Cmds |> Cmd.map totMsg }

            { new Machine<ElmishContainer<'ParentModel, 'ParentMsg>, 'Model * Cmd<'Msg>>() with
                member _.Setup = parentInit |> Gen.map create |> Arb.fromGen

                member __.Next model =
                    operations modelMapper toParentMsg totMsg model }

    let withUpdate
        update
        (f: Gen<'Parent>
                -> ('ParentModel -> 'ChildModel)
                -> ('ChildMsg -> 'ParentMsg)
                -> ('ParentMsg -> 'ChildMsg)
                -> ('ParentMsg -> 'ParentModel -> 'Parent)
                -> Machine<'ParentContainer, 'Child>)
        =
        fun init toState toParentMsg toMsg plainUpdate -> f init toState toParentMsg toMsg (update plainUpdate)


