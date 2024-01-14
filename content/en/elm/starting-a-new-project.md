---
title:                "Elm recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

If you're new to programming or just looking for a new language to try out, Elm could be the perfect choice for your next project. Elm is a functional programming language that focuses on creating reliable and maintainable front-end web applications. Its simple syntax and robust compiler make it a great language for both beginners and experienced developers alike.

## How To

To get started with Elm, you'll need to first install the Elm platform on your computer. This can be done easily by following the instructions on the official Elm website. Once you have Elm installed, you can start creating your first project.

To create a new project, you can use the `elm init` command in your terminal. This will create a basic project structure with a `Main.elm` file where you can start writing your code. Let's take a look at a simple `Hello World` example in Elm:

```Elm
import Html exposing (text)

main =
  text "Hello, World!"
```

Here, we're importing the `text` function from the `Html` package and using it to display the text "Hello, World!" on the screen. To run this code, you can use the `elm reactor` command and navigate to `http://localhost:8000` in your browser to see the output.

One of the great things about Elm is its type system. It ensures that your code is free of runtime errors and makes refactoring much easier. Let's take a look at another example using types:

```Elm
add : Int -> Int -> Int
add x y =
  x + y

main =
  add 5 4
```

In this code, we've defined a function `add` that takes in two `Int` values and returns their sum. The `main` function calls this `add` function with the values 5 and 4, and the result is displayed as 9 on the screen.

## Deep Dive

When starting a new project in Elm, it's important to understand its architecture. Elm follows the Model-View-Update (MVU) pattern which separates code into three distinct parts: Model represents the current state of the application, View is responsible for rendering the UI, and Update handles all the logic for updating the model based on user actions.

To learn more about the MVU pattern and other advanced concepts in Elm, it's recommended to explore the official documentation and community resources. It's also helpful to play around with existing projects and see how they've implemented different features.

## See Also

- [Elm Official Website](https://elm-lang.org/)
- [Elm Docs](https://guide.elm-lang.org/)
- [Elm Community Packages](https://package.elm-lang.org/)