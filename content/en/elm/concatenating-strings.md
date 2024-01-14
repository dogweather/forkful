---
title:                "Elm recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is a basic but crucial aspect of programming in any language, including Elm. It allows us to combine multiple strings together to create a new string, which is extremely useful for building dynamic and flexible applications.

## How To

To concatenate strings in Elm, we can use the **++** operator. Let's take a look at an example:

```Elm
name = "John"
greeting = "Hello, " ++ name ++ "!" 
```

In this code, we have declared a variable `name` with the value "John" and a variable `greeting` which is created by combining the string "Hello, " with the value of `name` and the exclamation mark. The final value of `greeting` would be "Hello, John!".

We can also use the **++** operator to add strings to the end of a list of strings. Here's an example:

```Elm
numbers = ["one", "two"]
moreNumbers = numbers ++ ["three", "four"]
```

In this case, the final value of `moreNumbers` would be ["one", "two", "three", "four"], as the **++** operator adds the new strings to the end of the list.

## Deep Dive

It's important to note that the **++** operator can only be used to concatenate strings or lists of strings. It cannot be used to combine other types of data such as numbers or booleans. If you try to use it with mixed types, you will receive an error.

Another important detail is that the **++** operator always creates a new string or list, rather than modifying the existing one. This is because strings in Elm are immutable, meaning they cannot be changed once they are created. This allows for safer and more predictable code.

We can also use the **concat** function to concatenate strings in Elm. It takes in a list of strings and returns a single string which is the result of concatenating all the strings in the list.

## See Also

- [Official Elm Documentation on Strings](https://elm-lang.org/docs/strings)
- [A Beginner's Guide to Elm Strings](https://guide.elm-lang.org/types/strings.html)
- [Mastering Elm: Concatenating Strings](https://thoughtbot.com/blog/mastering-elm-concatenating-strings)