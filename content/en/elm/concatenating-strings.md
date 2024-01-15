---
title:                "Concatenating strings"
html_title:           "Elm recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings may seem like a simple task, but it can greatly improve the functionality and readability of your code. By combining multiple strings into one, you can create dynamic content, generate personalized messages, and handle user input with ease. Plus, it's a fundamental skill that every Elm programmer should master.

## How To

To concatenate strings in Elm, you can use the `++` operator or the `String.concat` function. Let's take a look at some examples using both methods.

```
Elm code block:
import String

message1 = "Hello" ++ "World"
-- Output: "HelloWorld"

message2 = String.concat ["I", "love", "Elm"]
-- Output: "IloveElm"
```

In the first example, we used the `++` operator to combine two strings, "Hello" and "World", into one. In the second example, we used the `String.concat` function to join a list of strings into a single string. Both methods achieve the same result, so choose whichever one feels more intuitive to use.

You can also use concatenation to dynamically generate messages based on user input. Let's see an example of this using the `String.concat` function.

```
Elm code block:
import String

name = "John"
greeting = String.concat ["Hello", name, "!"]
-- Output: "Hello John!"
```

In this example, we stored the user's name in a variable called `name` and then used concatenation to create a personalized greeting. This is just one of many ways you can use concatenation to enhance the functionality of your code.

## Deep Dive

Concatenating strings may seem like a basic concept, but there are a few things to keep in mind when using it in your code. One important thing to note is that the order of the strings in the concatenation matters. Let's consider an example:

```
Elm code block:
reminder = "Don't forget" ++ "to" ++ "buy" ++ "milk"
-- Output: "Don't forgettobuymilk"
```

In this example, we tried to create a reminder using individual strings, but we forgot to add spaces in between. To fix this, we can add spaces within the strings or use the `String.intercalate` function to join the strings with a separator.

Another thing to keep in mind is that the data types of the strings being concatenated must match. If you try to combine a string with a number, Elm will throw an error. To avoid this, you can use the `toString` function to convert the number to a string before concatenating.

```
Elm code block:
message = "My favorite number is" ++ String.toString 7
-- Output: "My favorite number is 7"
```

Lastly, it's important to understand that strings are immutable in Elm, meaning they cannot be changed once created. This means that every time you use concatenation, a new string is created. So if you need to concatenate multiple strings, it's more efficient to use the `String.concat` function instead of using the `++` operator multiple times.

## See Also

- [Elm Guide: Strings](https://guide.elm-lang.org/types/strings.html)
- [Official Elm Documentation: String module](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Tutorial: Working With Strings](https://www.elm-tutorial.org/en/03-subs-cmds/03-working-with-strings.html)