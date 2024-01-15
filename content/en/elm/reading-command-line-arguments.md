---
title:                "Reading command line arguments"
html_title:           "Elm recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Command line arguments are a powerful tool for interacting with applications. They allow users to pass in information when executing a program, giving developers the ability to create more dynamic and customizable software.

## How To

Reading command line arguments in Elm is a simple and straightforward process. First, we need to import the `Cmd` and `Platform` modules. Then, we can use the `Platform.worker` function to create a program that accepts command line arguments.

```Elm
import Cmd exposing (Cmd)
import Platform exposing (worker)

main : Program () Model Msg
main =
  worker
    { init = init
    , update = update
    , subscriptions = subscriptions
    }
```

Next, we can use the `Platform.workerArgs` function to handle the command line arguments. This function takes a function as an argument, which will receive a list of strings representing the command line arguments.

```Elm
import Cmd exposing (Cmd)
import Platform exposing (worker, workerArgs)

main : Program String Model Msg
main =
  workerArgs
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

init : String -> (Model, Cmd Msg)
init args =
  ( Model args, Cmd.none )
```

Now, we can access the command line arguments within our `init` function and use them however we need to in our program. For example, we can print them out to the console.

```Elm
init : String -> (Model, Cmd Msg)
init args =
  ( Model args, Cmd.none )

type Msg
  = PrintArguments

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PrintArguments ->
      ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

Now, when we run our program with command line arguments, they will be printed to the console.

```
$ elm make Main.elm --output=main.js
$ node main.js arg1 arg2
["arg1", "arg2"]
```

## Deep Dive

In Elm, command line arguments are handled by the `Platform` module. By using the `workerArgs` function, we are creating a program that can receive command line arguments as its initial state. These arguments are then passed to our `init` function, allowing us to access and use them in our program.

It's also worth noting that the `Platform.workerArgs` function will automatically parse command line arguments into a list of strings, breaking at spaces. This is different from other languages, where command line arguments are usually separated by commas or other delimiters.

## See Also

- [Elm Documentation on Platform Module](https://package.elm-lang.org/packages/elm-lang/core/6.0.0/Platform)
- [Elm Documentation on Cmd Module](https://package.elm-lang.org/packages/elm-lang/core/6.0.0/Cmd)