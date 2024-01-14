---
title:                "Elm recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Why

If you've ever worked with command line interfaces, you know that reading command line arguments is an important skill to have. Whether you're automating tasks or building command line tools, understanding how to read and use command line arguments can greatly enhance your programming capabilities.

# How To

To read command line arguments in Elm, we use the Elm Platform library [`elm/core`](https://package.elm-lang.org/packages/elm/core/latest/). Within this library, we use the `Platform.worker` function to create a program that will handle command line inputs. Here's an example of how the code might look:

```
-- Import the necessary libraries
import Platform exposing (worker)
import Task
import String
import Json.Decode as Decode


-- Define a new type to represent command line arguments
type alias CommandLineArgs =
    { command : String
    , arguments : List String
    }


-- Create a task that will handle the command line arguments
parseCommandLineArgs : Task.Task x CommandLineArgs
parseCommandLineArgs =
    Task.perform Decode.decodeValue (Platform.worker windowDecode)


-- Define the decoder for command line arguments
windowDecode : Decode.Decoder CommandLineArgs
windowDecode =
    Decode.map2 CommandLineArgs
        (Decode.field "command" Decode.string)
        (Decode.field "arguments" (Decode.list Decode.string))


-- Finally, extract the command line arguments and print the output
main : Program Never
main =
    Platform.worker Init
        Noop


-- Define the init function
type alias Model =
    {}


type Msg
    = Init


init : ( Model, Cmd Msg )
init =
    ( Model
    , Cmd.map parseCommandLineArgs Task.perform
    )
```

You can test this code by compiling it and running the following command in your terminal:

```
elm make Main.elm --output=elm-bin
./elm-bin --command=build --arguments=src/Main.elm
```

The output should be `CommandLineArgs { command = "build", arguments = ["src/Main.elm"] }`.

# Deep Dive

Now, let's take a closer look at what's happening in this code. First, we define a custom type called `CommandLineArgs` to represent the command and its arguments. Then, we create a task called `parseCommandLineArgs` using `Task.perform` and a decoder to extract the data from the command line.

Next, in the `main` function, we use `Platform.worker` to call the `Init` message, which in turn triggers the `parseCommandLineArgs` task. Finally, the `init` function uses `Task.map` to convert the result of the task into a `Msg`. This allows us to handle the data and print it as desired.

Overall, reading command line arguments in Elm is a simple and straightforward process. By using the `elm/core` library and understanding how to create and handle tasks, you can easily incorporate this feature into your projects.

# See Also

- [Elm Platform Library](https://package.elm-lang.org/packages/elm/core/latest/)
- [Elm Guide - Commands](https://guide.elm-lang.org/effects/cmd.html)
- [Working with Command Line Arguments in Elm](https://thoughtbot.com/blog/working-with-command-line-arguments-in-elm)