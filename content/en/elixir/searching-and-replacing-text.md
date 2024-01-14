---
title:    "Elixir recipe: Searching and replacing text"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text is a common task in programming, especially when working with large datasets or multiple files. Using Elixir for this task can greatly simplify the process and improve efficiency.

## How To

To search and replace text in Elixir, we will use the `String.replace/4` function. This function takes in four arguments: the original string, the substring to search for, the replacement string, and a number indicating how many replacements should be made.

```
Elixir
original_string = "Hello, world!"
replaced_string = String.replace(original_string, "world", "Elixir")
IO.puts replaced_string
```

The above code will output "Hello, Elixir!" as the original string will have the substring "world" replaced with "Elixir".

We can also use regular expressions in the search and replace process. For example, if we wanted to replace all numbers in a string with an "x", we can do so using a regex pattern.

```
Elixir
original_string = "12345"
regex = ~r{\d+}
replaced_string = String.replace(original_string, regex, "x")
IO.puts replaced_string
```

The resulting output would be "xxxxx", as all the numbers in the original string have been replaced with "x".

## Deep Dive

The `String.replace/4` function is just one option for searching and replacing text in Elixir. There are also other functions such as `String.replace_at/3` and `String.replace_first/3` that offer different functionalities and advantages.

Additionally, Elixir also has built-in support for regular expressions using the `Regex` module. This allows for more advanced and specific patterns to be used in searching and replacing text.

## See Also

- [String.replace/4 documentation](https://hexdocs.pm/elixir/String.html#replace/4)
- [Regex module documentation](https://hexdocs.pm/elixir/Regex.html)
- [Elixir style guide](https://elixir-lang.org/getting-started/style-guide.html)