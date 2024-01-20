---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?
Starting a new project in Elm involves initializing a basic project structure allowing us to organize and manage our code effectively. Programmers do this since it's a straightforward way to jumpstart coding without dealing with time-consuming setup details.

## How to:
Let's create a new Elm project. Installing Elm is a prerequisite. Go to your terminal, and run:

```sh
npm install -g elm
```
Navigate to your workspace directory with `cd your-directory` and initialize a new Elm project.

```sh
elm init
```

This command sets up a new Elm project by creating an `elm.json` file which manages dependencies and a `src` directory where you'll put your code.

```Elm
// ./src/Main.elm
module Main exposing (..)

import Html exposing (Html, text)

main : Html msg
main =
    text "Hello, Elm!"
```

To run the above Elm application, type in the terminal:

```Sh
elm reactor
```

Copy the URL shown in the terminal and paste it in your browser to see your app running.

## Deep Dive:
Elm introduced `elm init` only in version 0.19. It aims to eradicate the manual file scrub-creating and configuring tasks, giving programmers a swift start. 

If you need more complex setups, frameworks like `create-elm-app` or `elm-webpack-starter` are alternatives, yielding project structures for larger applications. However, `elm init` is the official technique and typically the simplest one.

`elm init` creates a basic Elm 0.19 `elm.json` file behind the scenes. This file dictates your project dependencies and source directories. It is similar to `package.json` in Node.js ecosystem or `pom.xml` in Maven. 

## See Also:
Look into official Elm guide [Installation Guide | An Introduction to Elm](https://guide.elm-lang.org/install/elm.html) and Richard Feldman’s Elm tutorial for further details and more advanced concepts [Beginning Elm - A gentle introduction to the Elm language](http://elmprogramming.com/)