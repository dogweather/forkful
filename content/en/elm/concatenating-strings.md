---
title:    "Elm recipe: Concatenating strings"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

In programming, there are often times when we need to combine strings together to create a new string. This process is known as concatenation and it is a common operation in many programming languages. In this blog post, we will be focusing specifically on how to concatenate strings in Elm.

## How To

To concatenate strings in Elm, we can use the `++` operator. This operator takes two strings as arguments and returns a new string that is the result of the concatenation. Let's take a look at a simple example:

```Elm
"Hello, " ++ "World!" -- returns "Hello, World!"
```

We can also concatenate more than two strings by chaining multiple `++` operations together. Let's see an example of this:

```Elm
"Hello, " ++ "my " ++ "name " ++ "is " ++ "John!" -- returns "Hello, my name is John!"
```

Another way to concatenate strings in Elm is by using the `String.concat` function. This function takes in a list of strings and returns a single string that is the result of concatenating all the strings in the list. Let's see this function in action:

```Elm
String.concat ["Hello, ", "my ", "name ", "is ", "John!"] -- returns "Hello, my name is John!"
```

## Deep Dive

Behind the scenes, the `++` operator and the `String.concat` function are calling the `append` function from the standard library. This function takes in a `List (String)` and returns a single `String` that is the result of concatenating all the strings in the list. This is important to note because it means that the `++` operator and the `String.concat` function have the same performance.

It's also worth noting that strings in Elm are immutable, which means that they cannot be modified. So when we use the `++` operator or the `String.concat` function, we are actually creating a new string rather than modifying the original strings. This can have implications for performance, so it's important to be mindful of when and how we use concatenation in our programs.

## See Also

- [The Elm String documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Strings and Chars tutorial](https://elmprogramming.com/elm-strings-and-chars.html)
- [The `List` module in Elm](https://package.elm-lang.org/packages/elm/core/latest/List)