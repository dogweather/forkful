---
title:                "Logging"
aliases:
- /en/elm/logging.md
date:                  2024-01-25T02:03:50.226321-07:00
model:                 gpt-4-1106-preview
simple_title:         "Logging"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/logging.md"
---

{{< edit_this_page >}}

## What & Why?
Logging is essentially the process of recording events and data outputs from a piece of software as it runs, think of it as the software's diary. Programmers use logging to keep track of what's happening under the hood - it's invaluable for debugging issues, monitoring system behavior in real-time, and analyzing past activity for performance optimizations or audits.

## How to:
Elm's architecture doesn't support side effects like logging out of the box—you handle them through commands, which are a part of your application's architecture. For educational purposes, let's check how you could simulate logging by sending messages to JavaScript through ports.

First, you'll define a port module:

```Elm
port module Logger exposing (..)

-- Define a port to send logs out to JavaScript
port log : String -> Cmd msg
```

In your `Main.elm`, you'd use the `log` port to send out a log message:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- some updates to your model here
            ( updatedModel, log "AnEvent occurred." )

        AnotherEvent ->
            -- other model updates here
            ( anotherUpdatedModel, log "AnotherEvent occurred." )
```

On the JavaScript side, you would subscribe to the `log` port to handle the incoming log messages:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

Sample output in the JavaScript console would then be:

```
AnEvent occurred.
AnotherEvent occurred.
```

## Deep Dive
Traditionally, in languages like Python or Java, logging is done by using a logging library, which provides a straightforward API to log messages at various levels such as debug, info, warning, error, and critical.

Elm, with its focus on purity and immutability, doesn't provide this kind of direct logging, as any kind of IO or side effect is managed distinctly through the Elm architecture.

When you need full-featured logging in Elm, you typically rely on external JavaScript tools. Ports, as shown above, are the bridge to these tools. The Debug module is another option, but it's meant for development use only and not for production logging.

In addition to ports, programmers often make use of Elm's compiler messages and runtime debugging facilities, like `Debug.log`, which you can insert into your code to trace values. It wraps an expression and logs its output to the console like so:

```Elm
view model =
    Debug.log "Model Debug" model
    -- your view code here
```

This however also isn't meant for production. Tools like elm-logger provide some abstractions over ports for logging, although these are also meant for development more than production.

## See Also
- Elm ports: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Elm discourse on logging: https://discourse.elm-lang.org/t/elm-and-logging/546
- JavaScript Console API: https://developer.mozilla.org/en-US/docs/Web/API/Console
- elm-logger package: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
