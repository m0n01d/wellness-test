module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Extra as Html
import Html.Keyed as Keyed
import Json.Decode as Decode



-- Future UX improvements
-- Multiselect to move Todos
-- Find a nice way to collapse empty columns without them disappearing...
-- Tighten up types, something like AnyDict


main : Program () Model Msg
main =
    -- Start off with sandbox
    -- keep it simple..
    -- easy to refactor to other Browser programs
    -- but we're not making external reqs
    -- tbd..
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }



-- Represents all the Swimlanes in our app
-- and holds all the Todos


type alias Swimlane =
    Result
        Decode.Error
        { cols : Cols
        , todos : Todos
        }


type alias Model =
    { maybeForm : Maybe ( Status, InvalidForm ) -- Status determines the column the New todo will be in
    , swimlanes :
        Swimlane
    }



-- The different columns in our swimlane


type alias Cols =
    Dict String (List Id)


type alias Todos =
    Dict String Todo


initialForm : InvalidForm
initialForm =
    { assignee = "", description = "", title = "" }


initialModel : Model
initialModel =
    { maybeForm = Nothing
    , swimlanes = decodeSwimlanes
    }


view : Model -> Html Msg
view model =
    case model.swimlanes of
        Err err ->
            Html.div
                [ Attributes.class "px-4 py-3"
                ]
                [ Html.p
                    [ Attributes.class "text-red-800 mb-4"
                    ]
                    [ Html.text "One or more errors occured in your JSON." ]
                , Html.pre []
                    [ Html.text <| Decode.errorToString err
                    ]
                ]

        Ok { cols, todos } ->
            Html.div []
                [ Html.h1
                    [ Attributes.class "my-3 text-center text-3xl"
                    ]
                    [ Html.text "Todos" ]
                , viewSwimLanes model.maybeForm cols todos
                ]


viewSwimLanes : Maybe ( Status, InvalidForm ) -> Cols -> Todos -> Html Msg
viewSwimLanes maybeForm cols todos =
    (case Dict.get "To Do" cols of
        Nothing ->
            -- "To Do" should never be empty
            cols
                |> Dict.insert "To Do" []

        _ ->
            cols
    )
        |> Dict.toList
        |> List.reverse
        |> List.map (viewSwimLane maybeForm todos)
        |> Html.div
            [ Attributes.class "flex container flex-col md:flex-row mx-auto px-2 py-1"
            ]


viewSwimLane : Maybe ( Status, InvalidForm ) -> Todos -> ( String, List Id ) -> Html Msg
viewSwimLane maybeForm todos ( title, column ) =
    Html.div
        [ Attributes.class "px-4 py-2 flex-1 border border-neutral-600 mb-2"
        ]
        [ Html.p
            [ Attributes.class "border-b border-neutral-600 py-3 text-sm font-mono"
            ]
            [ Html.text title
            , Html.button
                [ Attributes.class "float-right"
                , Events.onClick <| ClickedAddTodo <| statusFromString title
                ]
                [ Html.text "+ Add Todo" ]
            ]
        , maybeForm
            |> Html.viewMaybe (viewAddTodoForm <| statusFromString title)
        , viewColumn (statusFromString title) column todos
        ]


viewAddTodoForm : Status -> ( Status, InvalidForm ) -> Html Msg
viewAddTodoForm columnStatus ( selectedStatus, form ) =
    Html.form
        [ Attributes.classList [ ( "py-4", True ), ( "hidden", selectedStatus /= columnStatus ) ]
        , Events.onSubmit <| SubmitNewTodo form columnStatus
        ]
        [ Html.label
            [ Attributes.class "flex"
            ]
            [ Html.text "Title:"
            , Html.input
                [ Attributes.class "ml-3 flex-1 border"
                , Attributes.value form.title
                , Events.onInput <| SetFormTitle ( selectedStatus, form )
                ]
                []
            ]
        , Html.fieldset
            [ Attributes.class "mt-2 flex"
            ]
            [ Html.label
                [ Attributes.class "flex"
                ]
                [ Html.text "Description:"
                ]
            , Html.textarea
                [ Attributes.class "ml-3 flex-1 border"
                , Attributes.value form.description
                , Events.onInput <| SetFormDescription ( selectedStatus, form )
                ]
                []
            ]
        , Html.fieldset
            [ Attributes.class "mt-2 flex"
            ]
            [ Html.label
                [ Attributes.class "flex flex-1"
                ]
                [ Html.text "Assignee:"
                , Html.input
                    [ Attributes.class "ml-3 flex-1 border"
                    , Attributes.value form.assignee
                    , Events.onInput <| SetFormAssignee ( selectedStatus, form )
                    ]
                    []
                ]
            ]
        , Html.button
            [ Attributes.class "mr-8 mt-3 border px-3 bg-amber-400 text-white py-2 font-semibold text-sm"
            , Attributes.type_ "button"
            , Events.onClick ClickedCancelAddTodo
            ]
            [ Html.text "Cancel" ]
        , Html.button
            [ Attributes.class " mt-3 border px-3 py-2 font-semibold text-sm"
            ]
            [ Html.text "Submit" ]
        ]


viewColumn : Status -> List Id -> Todos -> Html Msg
viewColumn title column todos =
    column
        |> List.map
            (\id ->
                ( idToString id, Html.viewMaybe (viewTodo title) (Dict.get (idToString id) todos) )
            )
        |> Keyed.node "div"
            [ Attributes.class "py-4"
            ]


viewTodo : Status -> Todo -> Html Msg
viewTodo column todo =
    Html.div
        [ Attributes.classList
            [ ( "border border-l-4 border-border-neutral-900 my-2 py-3 px-3 ", True )
            , ( statusToClassName todo.status, True )
            ]
        ]
        [ viewMoveMenu column todo
        , Html.p
            [ Attributes.class "font-semibold mb-3"
            ]
            [ Html.text todo.title
            ]
        , Html.p []
            [ Html.text todo.description
            ]
        , Html.p
            [ Attributes.class "mt-3 text-sm font-mono text-right text-slate-400"
            ]
            [ Html.text todo.assignee
            ]
        ]


viewMoveMenu : Status -> Todo -> Html Msg
viewMoveMenu columnStatus todo =
    Html.div
        [ Attributes.class "relative"
        ]
        [ Html.button
            [ Attributes.class "float-right text-xs"
            , Attributes.attribute "ui-button-toggle" ""
            ]
            [ Html.text "Move to.." ]
        , [ NeedTodo, InProgress, Done ]
            |> List.filter ((/=) columnStatus)
            |> List.map (viewMenuItem todo)
            |> Html.menu
                [ Attributes.class "hidden bg-white px-2 py-1 rounded border mt-6 w-32 absolute right-0 top-0"
                ]
        ]


viewMenuItem : Todo -> Status -> Html Msg
viewMenuItem todo columnStatus =
    Html.li
        [ Attributes.classList
            [ ( "border-l px-2 py-px my-1 hover:bg-slate-100", True )
            , ( statusToClassName columnStatus, True )
            ]
        ]
        [ Html.button
            [ Attributes.class "w-full text-left block"
            , Events.onClick <|
                MoveTodo columnStatus todo
            ]
            [ Html.text <| statusToString columnStatus ]
        ]


statusToClassName : Status -> String
statusToClassName status =
    case status of
        Done ->
            "border-l-green-500"

        InProgress ->
            "border-l-indigo-600"

        NeedTodo ->
            ""



-- update


type Msg
    = ClickedAddTodo Status
    | ClickedCancelAddTodo
    | MoveTodo Status Todo
    | SetFormAssignee ( Status, InvalidForm ) String
    | SetFormDescription ( Status, InvalidForm ) String
    | SetFormTitle ( Status, InvalidForm ) String
    | SubmitNewTodo InvalidForm Status


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedAddTodo status ->
            { model | maybeForm = Just ( status, initialForm ) }

        ClickedCancelAddTodo ->
            { model | maybeForm = Nothing }

        MoveTodo status todo ->
            { model
                | swimlanes =
                    Result.map
                        (\swimlanes ->
                            let
                                nextTodos =
                                    Dict.insert todo.title
                                        { todo
                                            | status = status
                                        }
                                        swimlanes.todos
                            in
                            { swimlanes
                                | cols = todosToCols (Dict.values nextTodos)
                                , todos = nextTodos
                            }
                        )
                        model.swimlanes
            }

        SetFormAssignee ( status, form ) assignee ->
            { model | maybeForm = Just ( status, { form | assignee = assignee } ) }

        SetFormDescription ( status, form ) description ->
            { model | maybeForm = Just ( status, { form | description = description } ) }

        SetFormTitle ( status, form ) title ->
            { model | maybeForm = Just ( status, { form | title = title } ) }

        SubmitNewTodo form status ->
            case parseForm form of
                Err err ->
                    model

                Ok { assignee, description, title } ->
                    let
                        id =
                            Id title

                        newTodo =
                            Todo assignee description id status title
                    in
                    { model
                        | maybeForm = Nothing
                        , swimlanes =
                            Result.map
                                (\swimlanes ->
                                    let
                                        nextTodos =
                                            Dict.insert title newTodo swimlanes.todos
                                    in
                                    { swimlanes
                                        | cols = todosToCols <| Dict.values nextTodos
                                        , todos = nextTodos
                                    }
                                )
                                model.swimlanes
                    }



-- Form


parseForm : InvalidForm -> Result String ValidForm
parseForm form =
    -- normally i would use
    -- https://package.elm-lang.org/packages/arowM/elm-form-decoder/latest/
    -- or similar to validate a form before POSTing
    -- but im skipping it for time
    Ok
        { assignee = form.assignee
        , description = form.description
        , title = form.title
        }


type alias InvalidForm =
    { assignee : String
    , description : String
    , title : String
    }


type alias ValidForm =
    { assignee : String
    , description : String
    , title : String
    }



-- Data


decodeSwimlanes : Swimlane
decodeSwimlanes =
    Decode.decodeString (Decode.list decodeTodo) json
        |> Result.map
            (\todos ->
                { cols = todosToCols todos
                , todos =
                    todos
                        |> List.map (\todo -> ( idToString todo.id, todo ))
                        |> Dict.fromList
                }
            )


todosToCols : List Todo -> Cols
todosToCols todos =
    todos
        |> List.foldl
            (\todo acc ->
                Dict.update (statusToString todo.status)
                    (\maybeCol ->
                        case maybeCol of
                            Nothing ->
                                Just [ todo.id ]

                            Just ids ->
                                Just <| todo.id :: ids
                    )
                    acc
            )
            Dict.empty


type Status
    = Done
    | InProgress
    | NeedTodo


statusToString : Status -> String
statusToString status =
    case status of
        Done ->
            "Done"

        InProgress ->
            "In Progress"

        NeedTodo ->
            "To Do"


statusFromString : String -> Status
statusFromString =
    \str ->
        case str of
            "Done" ->
                Done

            "In Progress" ->
                InProgress

            "To Do" ->
                NeedTodo

            _ ->
                NeedTodo


decodeStatus : Decode.Decoder Status
decodeStatus =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Done" ->
                        Decode.succeed Done

                    "In Progress" ->
                        Decode.succeed InProgress

                    "To Do" ->
                        Decode.succeed NeedTodo

                    _ ->
                        Decode.fail "Not a valid Status"
            )



-- normally this would go in its own module..


type Id
    = Id String


idToString : Id -> String
idToString (Id str) =
    str



-- Todo item


type alias Todo =
    { assignee : String
    , description : String
    , id : Id
    , status : Status
    , title : String
    }


decodeTodo : Decode.Decoder Todo
decodeTodo =
    Decode.map4
        (\assignee description status title ->
            { assignee = assignee
            , description = description

            -- mock json doesn't come with `id` fields
            -- so i make it up :shrug:
            , id = Id title
            , status = status
            , title = title
            }
        )
        (Decode.field "assignee" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "status" decodeStatus)
        (Decode.field "title" Decode.string)



-- Mock Data


json : String
json =
    """
[
  {
    "title": "Express Myself",
    "description": "Set the building on fire.",
    "status": "To Do",
    "assignee": "Lyla Harper"
  },
  {
    "title": "Catch Up Work - Saturday",
    "description": "Gonna need you to come into work on Saturday",
    "status": "In Progress",
    "assignee": "Hayes Aguirre"
  },
  {
    "title": "Catch Up Work - Sunday",
    "description": "Gonna need you to com into work on Sunday too.",
    "status": "In Progress",
    "assignee": "Ariah Koch"
  },
  {
    "title": "TPS Reports",
    "description": "Did you get the memo?",
    "status": "Done",
    "assignee": "Salvador Vega"
  },
  {
    "title": "Buy some more \\"Flare\\"",
    "description": "Apparently, 13 is not the minimum number of Flare.",
    "status": "Done",
    "assignee": "Dakota Calhoun"
  },
  {
    "title": "Move desk into storage room B.",
    "description": "See if you can take care of some of the rat problem while you're down here.",
    "status": "Done",
    "assignee": "Gary Crane"
  }
]
  """
