---
title:                "Elixir recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters matching a pattern in Elixir can be useful for cleaning up data or formatting strings in a certain way. It allows for more precise control over which characters are removed from a given string.

## How To

To delete characters matching a pattern in Elixir, we can use the `String.replace/4` function. This function takes in a string, a regular expression pattern, the replacement string, and the number of times the pattern should be replaced. Let's take a look at an example:

```Elixir
input = "h3ll0 w0rld"
String.replace(input, ~r/[a-z]/i, "")
```

In this example, we have a string that contains numbers and letters. We use the regular expression `[a-z]` to match all lowercase letters and the `i` option to ignore case. Then, we replace each match with an empty string, effectively deleting all lowercase letters from the input. The output of this code would be:

```Elixir
"30"
```

We can also use the `String.replace/3` function to delete characters without using regular expressions. This function simply takes in the string and the replacement string as arguments. Let's see how this would look:

```Elixir
input = "hello"
String.replace(input, "l", "")
```

In this example, we are deleting all occurrences of the letter "l" from the string "hello". The output would be:

```Elixir
"heo"
```

## Deep Dive

Under the hood, Elixir uses the `String.replace/4` function to implement the `String.delete/2` function. `String.delete/2` takes in a string and a list of characters to be deleted. It then converts this list to a regular expression and uses the `String.replace/4` function to perform the deletion. Knowing this, we can see that the `String.replace/4` function gives us more flexibility when it comes to deleting characters in Elixir.

## See Also

- [Elixir String module documentation](https://hexdocs.pm/elixir/String.html)
- [Regular expression cheat sheet](https://www.rexegg.com/regex-quickstart.html)

By using the `String.replace/4` function, we can easily delete characters matching a pattern in Elixir. This gives us more control and flexibility when working with strings. For more information, check out the Elixir documentation and experiment with different regular expressions to see the different ways in which characters can be deleted. Happy coding!