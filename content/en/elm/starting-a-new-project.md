---
title:                "Starting a new project"
html_title:           "Elm recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project in Elm means creating a fresh codebase for a new application or program. This allows programmers to start from a clean slate and build a project from scratch, tailored to their specific needs and goals. Starting a new project can also be a great opportunity to learn and practice new techniques and improve one's skills.

## How to:

To start a new project in Elm, first make sure you have the latest version of Elm installed. Then, create a new directory for your project and navigate to it in your command line. Once in the project directory, run the following command:

```
elm init
```

This will create a new Elm project with the basic file structure and dependencies. You can then open the project in your preferred code editor and start writing your code in the `src` directory. When you're ready to compile your code into a JavaScript file, run the following command:

```
elm make src/Main.elm --output=main.js
```

This will generate a `main.js` file in your project's directory, which you can then include in your HTML file to run your Elm code in the browser.

## Deep Dive:

Elm was created by Evan Czaplicki in 2012 as a functional programming language for building web applications. It was designed to be simple, fast, and reliable, with a focus on code readability and maintainability. Starting a new project in Elm is a great choice for developers who prioritize efficient and bug-free code.

As an alternative to starting a new project in Elm, some programmers may choose to use JavaScript or another popular web development language. However, these languages often have more complex syntax and can lead to more errors and bugs in the code. Elm's strict type system and functional approach help prevent common bugs and make refactoring and maintenance easier in the long run.

To implement a new project in Elm, it's important to understand the basics of functional programming and Elm's architecture. The Elm Architecture is a well-defined pattern for structuring Elm applications, which consists of a model, a view, and an update function. By following this pattern, developers can easily manage state and user interactions in their application.

## See Also:

- Official Elm Documentation: https://guide.elm-lang.org/
- Elm Cheat Sheet: https://devhints.io/elm
- Getting Started with Elm video series: https://www.youtube.com/watch?v=H-i-iP2QiJI