---
title:    "Elm recipe: Deleting characters matching a pattern"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why

When working with strings in programming, it is often necessary to manipulate them to fit a certain format or pattern. This could involve removing certain characters, such as numbers or symbols, from a string. In the case of Elm programming, knowing how to delete characters matching a pattern can come in handy when dealing with user input or formatting data before sending it to an API.

## How To

To delete characters matching a pattern in Elm, we can use the `replace` function from the `String` library. This function takes in the pattern we want to match and replace, as well as the new replacement string. Let's take a look at an example below:

```Elm
import String

originalString = "Hey $John! You have $100 in your account."

formattedString = String.replace "$" "" originalString 

```

In this example, we are removing the dollar sign from the original string and saving the formatted string in a new variable. The output of `formattedString` would be:

```
Hey John! You have 100 in your account.
```

Note that the `replace` function also supports using regular expressions as the pattern to match.

## Deep Dive

Behind the scenes, the `replace` function uses a powerful concept called functional programming. This means that rather than modifying the original string, a new string is created with the desired replacements. This helps with keeping the program code clean and reduces the chances of unexpected bugs.

Moreover, the `replace` function is a part of the `String` library, which provides many other useful functions for string manipulation in Elm. Taking the time to explore and familiarize yourself with this library can greatly improve your ability to handle strings in your Elm programs.

## See Also

- Elm String Library: https://package.elm-lang.org/packages/elm/core/latest/String