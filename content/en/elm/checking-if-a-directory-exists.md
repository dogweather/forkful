---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# An Informal Introduction to Elm: Checking If a Directory Exists

## What & Why?

Checking if a directory exists is just like peeking in your mailbox; it's checking to see whether a specific location on your system (a directory) is there or not. As coders, we do this to ensure that we're reading from or writing to a valid location, because if we don't, well...errors happen!

## How to:

Unfortunately, Elm, as a front-end focused language, does not have direct file or directory operations available. It can't interact directly with the filesystem due to the security model of the browser where Elm runs. You need to interact with a server using HTTP or through ports with JavaScript. Here's the JavaScript code to check if a directory exists:

```JavaScript
const fs = require('fs');

fs.access('/your-directory-path', fs.constants.F_OK, (err) => {
  console.log(`${err ? 'does not exist' : 'exists'}`);
});
```

And if you absolutely want to stick to Elm, here's how you can utilize JavaScript through ports:

```Elm
port module Main exposing (..)

port checkDirectory : String -> Cmd msg

port directoryExists : (Bool -> msg) -> Sub msg
```

In JavaScript, subscribe to `checkDirectory` and send result to `directoryExists`:

```JavaScript
const app = Elm.Main.init();
const fs = require('fs');

app.ports.checkDirectory.subscribe(function (path) 
{
    fs.access(path, fs.constants.F_OK, (err) => 
    {
        app.ports.directoryExists.send(!err);
    });
});
```

If the directory exists, the `directoryExists` will be true.

## Deep Dive

Historically, understanding filesystems wasn't front-and-center for most languages. Languages like C provided this ability early on because they had system-level ambitions. JavaScript - and therefore Elm, which compiles to JavaScript - originally targeted the browser, where security concerns make direct access to the filesystem a non-starter.

Even so, here are alternatives:
  * Menial: Ask the user to submit the directory through an input field.
  * Clever: Launch a server module (Node.js or Python, perhaps?), and ping it from your Elm app.

With regards to implementation, remember Elm ports are not like traditional function calls. Rather, we can think of them as subscription-based models that respond when something engaging happens.

## See Also:

- Learn more about Elm here: https://elm-lang.org/docs
- Understand Elm ports: https://guide.elm-lang.org/interop/ports.html
- Dive deeper into Node.js fs module: https://nodejs.org/api/fs.html