module Counter

open Browser.Dom
open Browser.Types
open FSharpDemos.Html

// ------------------------------------------------------------------------------------------------
// Implementing TODO list app with Fable
// ------------------------------------------------------------------------------------------------

// TODO #1: The sample code keeps track of the current text in the input
// box - understand how this works! Our model is just `string` and every
// time the input changes, we trigger `Input` event with a new value of the
// input (which we get from the element). This way, the current state is
// always the value in the textbox. We will need this later when adding new
// todo items to the list.

// TODO #2: Add support for adding items & rendering them:
//  * Modify the model so that it contains the current input (string)
//    together with a list of work items (list of strings) and update
//    the `initial` value and the `render` function to render `<li>` nodes
//  * Add a new event (`Create`) and a click handler for the button that
//    triggers it. Modify the `update` function to add the new item.

// TODO #3: Add support for removing items once they are done. To do this,
// we will need to track a unique ID to identify items. Add `NextId` to
// your `Model` record & increment it each time you add an item. Change the
// list of items to be of type `int * string` with ID and text and when
// the `X` button is clicked, trigger a new event `Remove(id)`. Then you
// just need to change the `update` function to remove the item from the list!

// BONUS: If you have more time, you can add support for marking items
// as completed - every item can be complete or incomplete and clicking
// on an item should swich the stats (use "class" => "done" to strike-through
// completed items). You can also add a view option for showing only
// complete/incomplete items.

// ------------------------------------------------------------------------------------------------
// Domain model - update events and application state
// ------------------------------------------------------------------------------------------------

type Item = { ID: int; Work: string; Done: bool }

type State =
    { Input: string
      Todos: List<Item>
      NextId: int }

type Event =
    | Input of string
    | Create
    | Remove of int
    | SwitchState of int

// ------------------------------------------------------------------------------------------------
// Given an old state and update event, produce a new state
// ------------------------------------------------------------------------------------------------

let update state evt =
    match evt with
    | Input s -> { state with Input = s }
    | Create ->
        { state with
            Todos =
                state.Todos
                @ [ { ID = state.NextId
                      Work = state.Input
                      Done = false } ]
            NextId = state.NextId + 1 }
    | Remove id ->
        { state with
            Todos = state.Todos |> List.filter (fun i -> i.ID <> id) }
    | SwitchState id ->
        { state with
            Todos =
                state.Todos
                |> List.map (fun i ->
                    { i with
                        Done = if i.ID = id then not i.Done else i.Done }) }

// ------------------------------------------------------------------------------------------------
// Render page based on the current state
// ------------------------------------------------------------------------------------------------


let renderItem trigger it =
    h?li
        [ "click" =!> fun _ _ -> trigger (SwitchState it.ID)
          "class" => if it.Done then "done" else "" ]
        [ text it.Work
          h?a [ "href" => "#"; "click" =!> fun _ _ -> trigger (Remove it.ID) ] [ h?span [] [ text "X" ] ] ]

let render trigger state =
    h?div
        []
        [ h?ul [] (List.map (renderItem trigger) state.Todos)
          h?input
              [ "value" => state.Input
                "input" =!> fun d _ -> trigger (Input(unbox<HTMLInputElement>(d).value)) ]
              []
          h?button [ "click" =!> fun _ _ -> trigger (Create) ] [ text "Add" ] ]

// ------------------------------------------------------------------------------------------------
// Start the application with initial state
// ------------------------------------------------------------------------------------------------

let init =
    { Input = ""
      Todos =
        [ { ID = -1
            Work = "Open TODO list"
            Done = true } ]
      NextId = 0 }

createVirtualDomApp "out" init render update
