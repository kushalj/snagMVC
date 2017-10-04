module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick)

import Json.Decode as Json


-- ellie at https://ellie-app.com/cCzwQH87Qa1/0


-- a snag

type alias Snag =
    { title     : String
    , completed : Bool
    , editing   : Bool
    , identifier : Int
    }


-- Filter state

type FilterState = All | Active | Completed


-- App State

type alias Model = 
    { snags   : List Snag
    , snag    : Snag
    , filter  : FilterState
    , nextIdentifier : Int
    }


type Msg
    = Add
    | Complete Snag
    | Uncomplete Snag
    | Delete Snag
    | ClearCompleted
    | UpdateField String
    | Filter FilterState


newSnag : Snag
newSnag =
    { title = ""
    , completed = False
    , editing = False
    , identifier = 0
    }


initialModel : Model
initialModel =
    { snags = 
        [ { title = "Kitchen ceiling nail pops"
            , completed = False
            , editing = False
            , identifier = 1
            }
        ]
    , snag  = { newSnag | identifier = 2 }
    , filter = All
    , nextIdentifier = 3
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
        { model
            | snags = model.snag :: model.snags
            , snag = { newSnag | identifier = model.nextIdentifier }
            , nextIdentifier = model.nextIdentifier + 1
            }

        Complete snag ->
            let
                updateSnag thisSnag =
                    if thisSnag.identifier == snag.identifier then
                        { snag | completed = True }
                    else
                        thisSnag
            in
                { model
                | snags = List.map updateSnag model.snags
                }

        Uncomplete snag ->
            let
                updateSnag thisSnag =
                    if thisSnag.identifier == snag.identifier then
                        { snag | completed = False }
                    else
                        thisSnag
            in
                { model
                | snags = List.map updateSnag model.snags
                }

        Delete snag ->
            { model | snags = List.filter (\mappedSnag -> snag.identifier /= mappedSnag.identifier) model.snags }
        
        ClearCompleted ->
             { model
                | snags = List.filter (\snag -> snag.completed == False) model.snags
             }

        UpdateField str ->
            let
                snag =
                    model.snag
                    
                updatedSnag =
                    { snag | title = str }
            in
                { model | snag = updatedSnag }

        Filter filterState ->
            { model | filter = filterState }



snagView : Snag -> Html Msg
snagView snag =
    let
        handleComplete =
        case snag.completed of
            True -> (\_ -> Uncomplete snag)
            False -> (\_ -> Complete snag)
    in
    li [ classList [ ( "completed", snag.completed ) ] ]
        [ div [class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked snag.completed 
                , onCheck handleComplete
                ]
                []
            , label [] [ text snag.title ]
            , button 
                [ class "destroy"
                , onClick (Delete snag)
                ]
                []
            ]
        ]


-- mockSnag : Snag
-- mockSnag =
--   { title = "A mock snag..."
--   , completed = False
--   , editing = False
--   }


-- handle Enter keypress

onEnter : Msg -> Attribute Msg
onEnter msg =
    let   
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
        Json.fail "not the right keycode"
    in
        on "keydown" (keyCode |> Json.andThen isEnter)


filteredSnags: Model -> List Snag
filteredSnags model =
    let
        matchesFilter =
            case model.filter of
                All ->
                    (\_ -> True)
                Active ->
                    (\snag -> snag.completed == False)
                Completed ->
                    (\snag -> snag.completed == True)
    in
        List.filter matchesFilter model.snags

view : Model -> Html Msg
view model =
  div []
      [ node "style" [ type_ "text/css" ] [ text styles ]
      , section [ class "snagapp" ]
            [ header [ class "header" ]
                [ h1 [] [ text "snags" ]
                , input
                    [ class "new-snag"
                    , placeholder "What needs to be done?"
                    , value model.snag.title
                    , autofocus True
                    , onEnter Add
                    , onInput UpdateField
                    ]
                    []
                ]
                , section [ class "main" ]
                    [ ul [ class "snag-list" ]
                        (List.map snagView (filteredSnags model))
                    ]
                , footer [ class "footer" ]
                    [ span [ class "snag-count" ]
                        [ strong []
                            [ text (toString (List.length (List.filter (\snag -> snag.completed == False) model.snags))) ]
                        , text " items left"
                        ]
                    , ul [ class "filters" ]
                        [ filterItemView model All 
                        , filterItemView model Active
                        , filterItemView model Completed
                        ]
                    , button
                        [ class "clear-completed" 
                        , onClick ClearCompleted
                        ]
                        [ text "Clear compeleted" ]
                    ] 
            ]
      ]


filterItemView : Model -> FilterState -> Html Msg
filterItemView model filterState =
    li []
        [ a
            [ classList [("selected", (model.filter == filterState) )]
            , href "#"
            , onClick (Filter filterState)
            ]
            [ text (toString filterState) ] 
        ]


main =
    Html.beginnerProgram
        { model = initialModel 
        , view = view
        , update = update
    }


styles : String
styles =
  -- I'll read the styles into this heredoc with the following in vim:
  -- :r https://raw.githubusercontent.com/tastejs/todomvc-app-css/master/index.css
  -- You can just copy/paste them into the heredoc if you prefer.
  """
  html,
  body {
    margin: 0;
    padding: 0;
  }

  button {
    margin: 0;
    padding: 0;
    border: 0;
    background: none;
    font-size: 100%;
    vertical-align: baseline;
    font-family: inherit;
    font-weight: inherit;
    color: inherit;
    -webkit-appearance: none;
    appearance: none;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
  }

  body {
    font: 14px 'Helvetica Neue', Helvetica, Arial, sans-serif;
    line-height: 1.4em;
    background: #f5f5f5;
    color: #4d4d4d;
    min-width: 230px;
    max-width: 550px;
    margin: 0 auto;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    font-weight: 300;
  }

  :focus {
    outline: 0;
  }

  .hidden {
    display: none;
  }

  .snagapp {
    background: #fff;
    margin: 130px 0 40px 0;
    position: relative;
    box-shadow: 0 2px 4px 0 rgba(0, 0, 0, 0.2),
                0 25px 50px 0 rgba(0, 0, 0, 0.1);
  }

  .snagapp input::-webkit-input-placeholder {
    font-style: italic;
    font-weight: 300;
    color: #e6e6e6;
  }

  .snagapp input::-moz-placeholder {
    font-style: italic;
    font-weight: 300;
    color: #e6e6e6;
  }

  .snagapp input::input-placeholder {
    font-style: italic;
    font-weight: 300;
    color: #e6e6e6;
  }

  .snagapp h1 {
    position: absolute;
    top: -155px;
    width: 100%;
    font-size: 100px;
    font-weight: 100;
    text-align: center;
    color: rgba(175, 47, 47, 0.15);
    -webkit-text-rendering: optimizeLegibility;
    -moz-text-rendering: optimizeLegibility;
    text-rendering: optimizeLegibility;
  }

  .new-snag,
  .edit {
    position: relative;
    margin: 0;
    width: 100%;
    font-size: 24px;
    font-family: inherit;
    font-weight: inherit;
    line-height: 1.4em;
    border: 0;
    color: inherit;
    padding: 6px;
    border: 1px solid #999;
    box-shadow: inset 0 -1px 5px 0 rgba(0, 0, 0, 0.2);
    box-sizing: border-box;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
  }

  .new-snag {
    padding: 16px 16px 16px 60px;
    border: none;
    background: rgba(0, 0, 0, 0.003);
    box-shadow: inset 0 -2px 1px rgba(0,0,0,0.03);
  }

  .main {
    position: relative;
    z-index: 2;
    border-top: 1px solid #e6e6e6;
  }

  .toggle-all {
    text-align: center;
    border: none; /* Mobile Safari */
    opacity: 0;
    position: absolute;
  }

  .toggle-all + label {
    width: 60px;
    height: 34px;
    font-size: 0;
    position: absolute;
    top: -52px;
    left: -13px;
    -webkit-transform: rotate(90deg);
    transform: rotate(90deg);
  }

  .toggle-all + label:before {
    content: '❯';
    font-size: 22px;
    color: #e6e6e6;
    padding: 10px 27px 10px 27px;
  }

  .toggle-all:checked + label:before {
    color: #737373;
  }

  .snag-list {
    margin: 0;
    padding: 0;
    list-style: none;
  }

  .snag-list li {
    position: relative;
    font-size: 24px;
    border-bottom: 1px solid #ededed;
  }

  .snag-list li:last-child {
    border-bottom: none;
  }

  .snag-list li.editing {
    border-bottom: none;
    padding: 0;
  }

  .snag-list li.editing .edit {
    display: block;
    width: 506px;
    padding: 12px 16px;
    margin: 0 0 0 43px;
  }

  .snag-list li.editing .view {
    display: none;
  }

  .snag-list li .toggle {
    text-align: center;
    width: 40px;
    /* auto, since non-WebKit browsers doesn't support input styling */
    height: auto;
    position: absolute;
    top: 0;
    bottom: 0;
    margin: auto 0;
    border: none; /* Mobile Safari */
    -webkit-appearance: none;
    appearance: none;
  }

  .snag-list li .toggle {
    opacity: 0;
  }

  .snag-list li .toggle + label {
    /*
      Firefox requires `#` to be escaped - https://bugzilla.mozilla.org/show_bug.cgi?id=922433
      IE and Edge requires *everything* to be escaped to render, so we do that instead of just the `#` - https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/7157459/
    */
    background-image: url('data:image/svg+xml;utf8,%3Csvg%20xmlns%3D%22http%3A//www.w3.org/2000/svg%22%20width%3D%2240%22%20height%3D%2240%22%20viewBox%3D%22-10%20-18%20100%20135%22%3E%3Ccircle%20cx%3D%2250%22%20cy%3D%2250%22%20r%3D%2250%22%20fill%3D%22none%22%20stroke%3D%22%23ededed%22%20stroke-width%3D%223%22/%3E%3C/svg%3E');
    background-repeat: no-repeat;
    background-position: center left;
  }

  .snag-list li .toggle:checked + label {
    background-image: url('data:image/svg+xml;utf8,%3Csvg%20xmlns%3D%22http%3A//www.w3.org/2000/svg%22%20width%3D%2240%22%20height%3D%2240%22%20viewBox%3D%22-10%20-18%20100%20135%22%3E%3Ccircle%20cx%3D%2250%22%20cy%3D%2250%22%20r%3D%2250%22%20fill%3D%22none%22%20stroke%3D%22%23bddad5%22%20stroke-width%3D%223%22/%3E%3Cpath%20fill%3D%22%235dc2af%22%20d%3D%22M72%2025L42%2071%2027%2056l-4%204%2020%2020%2034-52z%22/%3E%3C/svg%3E');
  }

  .snag-list li label {
    word-break: break-all;
    padding: 15px 15px 15px 60px;
    display: block;
    line-height: 1.2;
    transition: color 0.4s;
  }

  .snag-list li.completed label {
    color: #d9d9d9;
    text-decoration: line-through;
  }

  .snag-list li .destroy {
    display: none;
    position: absolute;
    top: 0;
    right: 10px;
    bottom: 0;
    width: 40px;
    height: 40px;
    margin: auto 0;
    font-size: 30px;
    color: #cc9a9a;
    margin-bottom: 11px;
    transition: color 0.2s ease-out;
  }

  .snag-list li .destroy:hover {
    color: #af5b5e;
  }

  .snag-list li .destroy:after {
    content: '×';
  }

  .snag-list li:hover .destroy {
    display: block;
  }

  .snag-list li .edit {
    display: none;
  }

  .snag-list li.editing:last-child {
    margin-bottom: -1px;
  }

  .footer {
    color: #777;
    padding: 10px 15px;
    height: 20px;
    text-align: center;
    border-top: 1px solid #e6e6e6;
  }

  .footer:before {
    content: '';
    position: absolute;
    right: 0;
    bottom: 0;
    left: 0;
    height: 50px;
    overflow: hidden;
    box-shadow: 0 1px 1px rgba(0, 0, 0, 0.2),
                0 8px 0 -3px #f6f6f6,
                0 9px 1px -3px rgba(0, 0, 0, 0.2),
                0 16px 0 -6px #f6f6f6,
                0 17px 2px -6px rgba(0, 0, 0, 0.2);
  }

  .snag-count {
    float: left;
    text-align: left;
  }

  .snag-count strong {
    font-weight: 300;
  }

  .filters {
    margin: 0;
    padding: 0;
    list-style: none;
    position: absolute;
    right: 0;
    left: 0;
  }

  .filters li {
    display: inline;
  }

  .filters li a {
    color: inherit;
    margin: 3px;
    padding: 3px 7px;
    text-decoration: none;
    border: 1px solid transparent;
    border-radius: 3px;
  }

  .filters li a:hover {
    border-color: rgba(175, 47, 47, 0.1);
  }

  .filters li a.selected {
    border-color: rgba(175, 47, 47, 0.2);
  }

  .clear-completed,
  html .clear-completed:active {
    float: right;
    position: relative;
    line-height: 20px;
    text-decoration: none;
    cursor: pointer;
  }

  .clear-completed:hover {
    text-decoration: underline;
  }

  .info {
    margin: 65px auto 0;
    color: #bfbfbf;
    font-size: 10px;
    text-shadow: 0 1px 0 rgba(255, 255, 255, 0.5);
    text-align: center;
  }

  .info p {
    line-height: 1;
  }

  .info a {
    color: inherit;
    text-decoration: none;
    font-weight: 400;
  }

  .info a:hover {
    text-decoration: underline;
  }

  /*
    Hack to remove background from Mobile Safari.
    Can't use it globally since it destroys checkboxes in Firefox
  */
  @media screen and (-webkit-min-device-pixel-ratio:0) {
    .toggle-all,
    .snag-list li .toggle {
      background: none;
    }

    .snag-list li .toggle {
      height: 40px;
    }
  }

  @media (max-width: 430px) {
    .footer {
      height: 50px;
    }

    .filters {
      bottom: 10px;
    }
  }
  """
