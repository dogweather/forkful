---
title:                "Starting a new project"
html_title:           "Elm recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Starting a new project in Elm can be an exciting venture for developers. This functional programming language offers a unique and elegant approach to building web applications, making it a popular choice among web developers. With its ease of use, strong community, and excellent documentation, choosing to start a new project in Elm can lead to a smooth and successful development process.

## How To

Here is a simple example of how to create a "Hello World" program in Elm:

```Elm
module Main exposing (..)

import Html exposing (Html, text)

main : Html msg
main =
    text "Hello, World!"
```

In this code block, we first declare the module name and the functions that will be accessible to it. Next, we import the `Html` library, which contains functions for creating HTML elements. Then, we define our `main` function, which returns an HTML element containing the text "Hello, World!". Finally, we compile and run our program in the Elm repl, and we should see the text "Hello, World!" displayed on the screen.

Elm has a unique approach to handling HTML elements, using a virtual DOM to efficiently update the page when changes are made. Here is an example of how to create a simple HTML form in Elm:

```Elm
module Main exposing (..)

import Html exposing (Html, button, div, form, label, input, text)

type alias Model =
    { name : String
    , age : Int
    }

type Msg
    = UpdateName String
    | UpdateAge String

init : Model
init =
    { name = ""
    , age = 0
    }

view : Model -> Html Msg
view model =
    form []
        [ label [] [ text "Name:" ]
        , input [ onInput UpdateName ] []
        , label [] [ text "Age:" ]
        , input [ onInput UpdateAge ] []
        , div [] [ text ("Hello, " ++ model.name ++ "! You are " ++ String.fromInt model.age ++ " years old.") ]
        , button [] [ text "Submit" ]
        ]

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateName newName ->
            { model | name = newName }

        UpdateAge newAge ->
            { model | age = Maybe.withDefault 0 (String.toInt newAge) }

main : Html msg
main =
    view init
```

In this example, we create a simple model with two fields - `name` and `age` - and two message types - `UpdateName` and `UpdateAge`. The `view` function takes in the model and returns an HTML form with two input fields and a button. The `update` function handles the events from the input fields and updates the model accordingly. Finally, the `main` function calls the `view` function with the initial model.

## Deep Dive

When starting a new project in Elm, it is important to familiarize yourself with the language's basic concepts, such as static typing and immutability. Elm also has a helpful community and a package manager called `elm-package`, which makes it easy to access third-party libraries.

Another important aspect to consider when starting a new project is the Elm architecture. This architecture follows a `model-update-view` pattern, where the model serves as the single source of truth, the update function handles all the changes to the model, and the view function creates the HTML representation of the model.

Additionally, Elm has a feature called "time-travel debugging", which allows developers to step through each state of their application and easily debug any issues.

## See Also

- Official Elm documentation: https://elm-lang.org/docs
- Elm packages: https://package.elm-lang.org/
- Elm forum: https://discourse.elm-lang.org/