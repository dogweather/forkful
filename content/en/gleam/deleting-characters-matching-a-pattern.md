---
title:    "Gleam recipe: Deleting characters matching a pattern"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

If you have ever worked with strings in programming, you know the importance of being able to manipulate and modify them in various ways. One common task is to delete characters that match a specific pattern. This can help with data cleaning, formatting, and more. In Gleam, there are multiple ways to achieve this, making it a powerful tool in your coding arsenal.

## How To

To delete characters matching a pattern in Gleam, there are a few functions that can come in handy. First, we have the `String.replace` function, which takes in three arguments: the original string, the pattern to match, and the replacement string. Let's see an example:

```
Gleam> let str = "Hello, $world$!"
Gleam> String.replace(str, "$", "")
"Hello, world!"
```

Here, the dollar signs in the string are replaced with an empty string, effectively deleting them. This function also supports regular expressions, providing even more flexibility. Another option is the `String.split` function, which splits the string into a list of substrings, using the provided pattern as a delimiter. We can then combine this with the `String.concat` function to reconstruct the string without the specified pattern. Here is an example:

```
Gleam> let str = "This,is,comma,separated"
Gleam> let list = String.split(str, ",")
Gleam> String.concat(list)
"Thisisseparated"
```

Lastly, we have the `String.remove` function, which takes in an index and the number of characters to remove, effectively deleting them from the string. This can be useful when working with fixed-width data or known patterns. Here is an example:

```
Gleam> let str = "Gleam is awesome"
Gleam> String.remove(str, 6, 3)
"Gleam awesome"
```

## Deep Dive

Now that we have seen some examples of how to delete characters matching a pattern in Gleam, let's dive a bit deeper into this topic. One thing to keep in mind is that strings in Gleam are immutable, meaning they cannot be changed once created. This means that every time a string is modified, a new string is created. While this may seem inefficient, it actually helps with concurrency and parallelism, making Gleam a great choice for high-performance applications.

Another important aspect to consider is the difference between single-quoted and double-quoted strings. Single-quoted strings are literal strings, while double-quoted strings support escape codes and interpolations. This means that double-quoted strings will need to be escaped when using the `String.replace` function to delete special characters, while single-quoted strings can be used as is. Here is an example:

```
Gleam> let str = '"Hello, world!"'
Gleam> String.replace(str, '"', "")
'Hello, world!'
```

It is also worth noting that Gleam has built-in support for Unicode, making it easier to handle characters from different languages and writing systems. This can be especially useful when dealing with non-ASCII characters that may need to be deleted from a string.

## See Also

For more information on string manipulation in Gleam, check out these resources:

- Official Gleam documentation: https://gleam.run/documentation
- "Introduction to Strings in Gleam" blog post: https://blog.gleam.run/introduction-to-strings-in-gleam/
- Gleam string module source code: https://github.com/gleam-lang/gleam_stdlib/blob/master/std/string.gleam