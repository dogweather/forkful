---
title:    "Elm recipe: Starting a new project"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Starting a new project with Elm is a great way to create modern, robust and reliable web applications. With its functional programming language and built-in static type system, Elm allows you to write code that is easy to maintain and test, making it a popular choice for both small and large-scale projects.

## How To
Creating a new Elm project is a straightforward process. First, make sure you have Elm installed on your machine. Then, open your preferred code editor and follow these steps:

1. Create a new directory for your project.
2. Navigate to the newly created directory in your terminal.
3. Run the command `elm init` to initialize the project.
4. You can now start writing your code in the `src` directory.

Let's take a look at a simple example of defining a function in Elm:

```Elm
double : Int -> Int
double x =
    x * 2
```

In this code block, we have defined a function `double` that takes in an integer and returns its doubled value. The `->` arrow denotes the return type of the function.

Now, let's see how we can call this function and print out the result:

```Elm
main =
    double 5 |> Debug.log
```

The `main` function is the starting point of every Elm program. Here, we are calling our `double` function with an argument of `5` and using the `Debug.log` function to print the output to the console.

Running the Elm code above will give us an output of `10` in the console.

## Deep Dive
In addition to its functional nature and static type system, Elm also offers features such as virtual DOM, time-travel debugging, and automatic code formatting that make it a powerful and efficient language for developing user interfaces. It also has a helpful and supportive community that offers resources and support for new users.

If you want to learn more about starting a new Elm project, be sure to check out the official documentation and tutorials available online.

## See Also
- Official Elm Documentation: https://guide.elm-lang.org/
- Elm Tutorials: https://elm-tutorial.org/
- Elm Community Forum: https://discourse.elm-lang.org/