---
title:                "Elm recipe: Starting a new project"
programming_language: "Elm"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Starting a new project can be daunting, but with Elm, it doesn't have to be. Elm is a functional programming language that allows for easy, scalable, and maintainable code. It also has a friendly and helpful community, making it a great choice for building web applications.

## How To

To start a new project in Elm, you will need to have Elm installed on your computer. Once you have Elm installed, you can follow these steps:

1. Create a new directory for your project and navigate to it in your terminal.
2. Run the `elm init` command to set up your project's `elm.json` file.
3. Start your project by creating a `Main.elm` file that contains your main function. This function should return a `Html msg` value, which is the starting point for your application.
4. Write your code using the Elm syntax, which is simple and easy to understand. Keep in mind that Elm is a strongly typed language, so you will need to define your data types.
5. Use the Elm REPL (Read-Eval-Print-Loop) to test your code and make sure it is working as expected.
6. Once you are satisfied with your code, run the `elm reactor` command to start a local server and view your application in the browser.

Here is an example of a simple Elm program:

```Elm
module Main exposing (main)

import Html exposing (text)

main =
  text "Hello, world!"
```

Running this code will display the text "Hello, world!" on your localhost:8000 port.

## Deep Dive

When starting a new project in Elm, it is important to keep in mind that it follows the "model-view-update" (MVU) architecture. This means that the state of your application is managed by a model, and changes to the model are made through update functions. View functions then use the model to render the user interface.

Additionally, Elm has a built-in package manager called `elm package`. This allows you to easily add external packages to your project, making it easy to incorporate features and functionality from other developers.

Another great aspect of Elm is its error messages. Instead of cryptic lines of code, Elm provides clear and helpful error messages that make debugging much easier.

## See Also

- [Official Elm website](https://elm-lang.org/)
- [Elm Guide](https://guide.elm-lang.org/)
- [Elm Packages](https://package.elm-lang.org/)