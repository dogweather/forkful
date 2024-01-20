---
title:                "Writing to standard error"
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to standard error (stderr) is outputting error messages and diagnostics separate from regular output. Programmers do it to debug and monitor applications without mixing error messages with standard output (stdout).

## How to:
Elm runs on the web, and browsers don't differentiate between stdout and stderr like command-line interfaces do. However, you can simulate stderr using JavaScript interop through ports. Here's how to set it up:

```Elm
port module Main exposing (..)

import Html

-- Define a port to send error messages to JavaScript
port stderr : String -> Cmd msg

-- Function to simulate writing to stderr
writeToStdErr : String -> Cmd msg
writeToStdErr message =
    stderr message

main =
    writeToStdErr "Error: Something went wrong"
    |> Html.programWithFlags { init = \_ -> ((), Cmd.none), update = \_ _ -> ((), Cmd.none), view = \_ -> Html.text "", subscriptions = \_ -> Sub.none }
```

And the corresponding JavaScript:

```JavaScript
var app = Elm.Main.init();

// Listen for errors on the 'stderr' port and log them to the console as errors
app.ports.stderr.subscribe(function(message) {
    console.error(message);
});
```

Sample output in the browser console:

```
Error: Something went wrong
```

## Deep Dive
Historically, stderr is a Unix concept where output streams are categorized for better process control and automation. Elm, being primarily a frontend language, doesn't have built-in support for this concept since web applications typically handle errors within the UI or via network operations, not through a terminal. Alternatives for debugging in Elm include using the Elm Debugger, which visually presents the state of your application. Behind the ports, Elm's JavaScript interop constructs messages that JavaScript subscribes to, essentially bridging the gap between Elm and traditional stderr.

## See Also
- Elm's official guide on ports: https://guide.elm-lang.org/interop/ports.html
- Elm Debugger: https://guide.elm-lang.org/effects/debugging.html
- Writing cross-platform stdout and stderr in Node.js: https://nodejs.org/api/console.html