---
date: 2024-01-20 18:03:19.508926-07:00
description: "Starting a new project in Elm is about setting up a clean slate for\
  \ building reliable web apps. Programmers do it to capitalize on Elm's simplicity\
  \ and\u2026"
lastmod: '2024-03-13T22:45:00.010545-06:00'
model: gpt-4-1106-preview
summary: "Starting a new project in Elm is about setting up a clean slate for building\
  \ reliable web apps. Programmers do it to capitalize on Elm's simplicity and\u2026"
title: Starting a new project
---

{{< edit_this_page >}}

## What & Why?

Starting a new project in Elm is about setting up a clean slate for building reliable web apps. Programmers do it to capitalize on Elm's simplicity and robustness, especially for projects demanding no runtime exceptions.

## How to:

In Elm, kick things off with the `elm init` command. Navigate to your project directory and fire up your terminal:

```shell
mkdir my-elm-project
cd my-elm-project
elm init
```

This command creates an `elm.json` file and `src` directory. Here’s a simple "Hello, World!" in Elm:

```Elm
module Main exposing (..)

import Html exposing (text)

main =
    text "Hello, World!"
```

When you run it with `elm reactor` and visit `http://localhost:8000`, it's going to show "Hello, World!" in your browser.

## Deep Dive

Elm came around in 2012, aiming to make front-end development more pleasant. It's not just about the avoidance of runtime errors; Elm brings a strong focus on simplicity and developer happiness. Unlike many alternatives, such as writing raw JavaScript or using frameworks like React, Elm is a language of its own. With strong typing and pure functions, it brings predictability and maintainability to the table.

When you start a new Elm project you're also embracing the Elm Architecture, a pattern for structuring your web apps which emphasizes simplicity and scalability. It bundles up your entire application state and how it updates. Other tools like `create-elm-app` can scaffold more complex setups, but starting with `elm init` is as lean as it gets.

## See Also

- Elm Official Guide: https://guide.elm-lang.org/
- Elm Architecture Tutorial: https://guide.elm-lang.org/architecture/
- Elm Tooling: `create-elm-app`: https://github.com/halfzebra/create-elm-app
- Elm Package Catalog: https://package.elm-lang.org/
