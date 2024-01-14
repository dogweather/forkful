---
title:    "Elm recipe: Reading command line arguments"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why

Command line arguments are an essential part of any programming language, including Elm. They allow us to pass in specific information to our program when it is executed. By learning how to read command line arguments, we can make our Elm programs more interactive and flexible.

## How To

To read command line arguments in Elm, we will be using a function called `map`. This function takes in a list of arguments and a function. It then applies that function to each argument in the list and returns a new list with the transformed values.

To demonstrate this, let's create a simple program that takes in two arguments and adds them together. Our initial code would look like this:

```Elm
module Main exposing (..)

import String

main : Program Never Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = always Sub.none
        }

type alias Model =
    { sum : Int
    }

type Msg
    = Add Int Int
    | NoOp

init : () -> ( Model, Cmd Msg )
init _ =
    ( { sum = 0 }, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add num1 num2 ->
            ( { model | sum = num1 + num2 }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

```

Next, we need to modify our `update` function to handle the `Add` message. We will use the `map` function to transform our list of arguments into integers and then add them together. The updated code would look like this:

```Elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add num1 num2 ->
            let
                --transform arguments to integers
                num1Int = String.toInt num1
                num2Int = String.toInt num2
            in
                ( { model | sum = num1Int + num2Int }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )
```

Now we can run our program with arguments from the command line by using the `elm reactor` command. For example, if we run `elm reactor -a 5 10`, our program will add 5 and 10 together and return the result.

## Deep Dive

The `map` function is just one way to read command line arguments in Elm. Another option is to use the `Cmd` module, which provides functions for executing commands from the command line. It also allows us to pass in flags and resolve them into Elm values.

Additionally, we can use the `Platform` module to access command line arguments in browser-based applications. This is useful for creating more interactive web applications that can take in user input from the command line.

## See Also

For more information on Elm and command line arguments, check out these useful resources:

- [Elm documentation on command line arguments](https://package.elm-lang.org/packages/elm/core/latest/Platform)
- [Using flags and commands in Elm](https://elmprogramming.com/flags-and-commands.html)
- [Creating interactive web applications with Elm](https://guide.elm-lang.org/architecture/effects/flags.html)