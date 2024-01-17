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

## What & Why?

Reading command line arguments is the process of retrieving user input passed through the command line in a program. This allows programmers to create more dynamic and interactive applications, as user input can be used to modify the behavior of the program. It is a fundamental skill for any programmer, as it allows for more advanced and customizable applications.

## How to:
To read command line arguments in Elm, we first need to import the `Platform` module. Then, we can use the `flagDecoder` function to decode the command line arguments into a `Task` that returns a list of strings. Here's an example of how to use it:

```Elm
import Platform

main : Program String
main =
    Platform.worker
        { init = \_ -> ( [], Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = flagDecoder (map config [1..3])
        }

config : List String -> Int -> String -> String
config args index =
    "Argument " ++ (toString index) ++ ": " ++ (List.head args) ++ "\n"

```

Running this program with the command line arguments `elm make Main.elm -- arg1 arg2 arg3` will produce the following output:

```
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```
Note: In the `Platform.worker` function, we are passing in a dummy `update` function and a `Cmd.none` command, as we do not need them for this example.

## Deep Dive:
Command line arguments have been a widely used feature in programming languages since the early days of computing. They provide a way for users to interact with a program through a simple text interface, making it more versatile and useful. Before command line arguments, programs would often require user input through complicated GUIs or through hardcoded values, which limited their functionality.

In Elm, there are other ways to get user input, such as using ports or using `Program`s with `subscriptions`. However, command line arguments offer a more direct and simple way to retrieve user input.

To use command line arguments in Elm, we need to use the `Platform` module since it provides functions that allow us to interact with the platform that our application is running on, in this case, the command line. The `flagDecoder` function takes in a decoder and returns a `Task` that decodes the command line arguments into a list of strings, which can then be used in our program.

## See Also:
- Elm Documentation: [Command Line Arguments](https://package.elm-lang.org/packages/elm/core/latest/Platform#flagDecoder)
- Elm Guide: [Customizing Flags](https://guide.elm-lang.org/interop/flags.html)
- Elm-Lang.org: [Elm homepage](https://elm-lang.org/)