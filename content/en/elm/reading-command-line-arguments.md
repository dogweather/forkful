---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Command line arguments are inputs you send to your program when running it from a console or terminal. They're a program's lifeblood, used for things like file paths, settings, debugging, and more.

## How to:

Let's get real, Elm isn't built for command-line apps. But, with Node.js and the Ports feature, we can do some tricks. Here's a way to read command-line arguments:

1. We'll need to install `elm` and `elm-node` packages if you haven't yet. In your terminal, fire off these commands:

```shell
npm install elm
npm install elm-node
```

2. Create an `Elm` file, name it `Main.elm`:

```Elm
port module Main exposing (..)

port cmdArgs : (List String -> msg) -> Sub msg
```

3. Next, the `index.js` file:

```JavaScript
const { Elm } = require('./Main.elm');
const app = Elm.Main.init();

app.ports.cmdArgs.subscribe(function() {
    process.stdout.write(process.argv);
});
```

4. Now, let’s compile and run it:

```shell
npx elm make Main.elm --output=Main.js
node index.js hello world
```

You should see `['node', 'index.js', 'hello', 'world']` as output.

## Deep Dive

Historically, Elm's aimed at front-end development. Command line apps is not its forte. Elm 0.19 removed native modules, making it trickier to access command line arguments natively.

You have choices, though. You could use JavaScript interop (like we did), or you could use a different language more suited for command-line applications, such as Python or Ruby.

Ports give us a way to communicate with JavaScript. In our example, we're sending the command-line arguments from `index.js` to `Main.elm` via the `cmdArgs` port.

## See Also

Elm's documentation on [Ports](https://guide.elm-lang.org/interop/ports.html)  
Node.js guide on [command-line arguments](https://nodejs.dev/learn/nodejs-accept-arguments-from-the-command-line)  
Useful Elm packages for [Node.js](https://package.elm-lang.org/packages/eeue56/elm-serverless/latest/) and more on [GitHub](https://github.com/sporto/elm-node)