---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String interpolation is the process of evaluating expressions within string literals to create a new string. It makes your life easier when you need to inject dynamic data into static strings, and it makes your code cleaner too.

## How to:

In current versions of Elm, you typically concatenate strings and expressions using the `++` operator:

```Elm
main =
    let
        name = "John Doe"
    in
    Text.fromString ("Hello, " ++ name ++ "!")
```

Output:

```
Hello, John Doe!
```

## Deep Dive

Historically, some languages have provided special syntax for string interpolation, but Elm has chosen a simpler path. While direct string interpolation mechanisms aren't part of Elm, you can utilize functions to emulate similar functionality. For instance:

```Elm
greeting name =
    "Hello, " ++ name ++ "!"
```

One benefit of this approach is the increased control over how your data gets interpolated. It pushes you to think about data conversion and prevents potential type mismatch issues.

As for alternatives, if you're handling complex strings with many variables, consider using `List.join` or `String.concat`:

```Elm
sentence name age =
    List.join ["Hello, ", name, "! You are ", String.fromInt age, " years old."]
```

## See Also

- [The Official Elm Guide to Strings](https://guide.elm-lang.org/interop/strings.html): Dive deeper into Elm's approach to strings and concatenation.
- [Elm's Syntax Documentation](https://elm-lang.org/docs/syntax): Brush up on how Elm structures its syntax, including how it handles strings.
- [Discussion on String Interpolation in Elm](https://discourse.elm-lang.org/t/the-absence-of-string-interpolation/6676): Community discussion on why Elm doesn't have built-in string interpolation.