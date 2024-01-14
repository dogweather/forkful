---
title:    "Elm recipe: Concatenating strings"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why 

Concatenating strings is a useful skill to have in any programming language, including Elm. It allows you to combine multiple strings into one, making it easier to manipulate and display data. Whether you are building a simple to-do list app or a complex data visualization tool, knowing how to concatenate strings in Elm will come in handy.

## How To 

Concatenating strings in Elm is a straightforward process. You can use the `++` operator to combine two strings. Let's look at an example of how to use it:

```elm
full_name : String
full_name =
  "John" ++ " " ++ "Doe"
```

The `full_name` string will now have the value of "John Doe". You can also concatenate more than two strings at a time:

```elm
greeting : String
greeting =
  "Hello" ++ " " ++ "there" ++ " " ++ "!" -- "Hello there!"
```

You can also concatenate variables and strings together:

```elm
name : String
name = "Jane"

greet_name : String
greet_name = "Hello, " ++ name -- "Hello, Jane"
```

As you can see, concatenating strings is as simple as using the `++` operator between the strings you want to combine.

To add a line break between concatenated strings, you can use the `++ "\n"` syntax. For example:

```elm
address : String
address =
  "123 Main St." ++ "\n" ++ "New York City" -- "123 Main St.
                                            -- New York City"
```

## Deep Dive 

It's important to note that in Elm, strings are immutable, which means they cannot be changed. This means that each time you concatenate strings, a new string is created instead of modifying the existing one. This may seem inefficient, but it helps with code clarity and prevents errors.

There are also other ways to concatenate strings in Elm, such as using the `append` and `join` functions from the `String` module. These functions provide more flexibility and customization options when it comes to concatenating strings.

It's important to handle special characters properly when concatenating strings in Elm. For example, if you want to add quotation marks around a string, you need to use the `String.fromChar` function to convert the character into a string before concatenating it.

## See Also 

For more information on concatenating strings in Elm and other useful string operations, check out the official Elm documentation:

- [String Module - Elm Docs](https://elm-lang.org/docs/string)
- [Elm Official Website](https://elm-lang.org/)
- [Elm Slack Community](https://elmlang.herokuapp.com/)

Happy coding!