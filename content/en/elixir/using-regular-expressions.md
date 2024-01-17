---
title:                "Using regular expressions"
html_title:           "Elixir recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular expressions are a powerful tool in programming that allows for pattern matching within strings. Developers use them to efficiently search, replace, and extract data from text in their applications.

## How to:

Using regular expressions in Elixir is simple and straightforward. Here's an example of using the `=~` operator to match a pattern within a string:

```
Elixir iex> "Hello World" =~ ~r/llo/
```

The output will be `true` since the pattern `llo` is found within the string "Hello World". You can also use regular expressions in functions, like this:

```
Elixir defmodule Test do
  def get_numbers(string) do
    regex = ~r/\d+/
    Regex.scan(regex, string)
  end
end
```

In this example, the `get_numbers` function will use the `~r/` notation to specify the regular expression pattern and then use the `Regex.scan` function to find all matching numbers within the given string.

## Deep Dive:

Regular expressions have been around since the 1950s and have been implemented in various languages. They are commonly used in text editors and command-line tools to find and manipulate text. In Elixir, regular expressions are implemented using the PCRE (Perl Compatible Regular Expressions) library.

There are alternatives to using regular expressions, such as Elixir's own pattern matching capabilities. However, regular expressions offer a more flexible and powerful way to handle string manipulation.

When using regular expressions, it's important to understand the underlying principles and syntax to avoid making mistakes. Familiarizing yourself with metacharacters, anchors, and quantifiers can greatly improve your efficiency in writing regular expressions.

## See Also:

To learn more about using regular expressions in Elixir, check out the official documentation [here](https://hexdocs.pm/elixir/Regex.html). You can also explore other resources like tutorials and examples on sites like [Elixir School](https://elixirschool.com/en/lessons/basics/regex/) and [RegexOne](https://regexone.com/).

Now that you have a basic understanding of regular expressions in Elixir, go ahead and start using them in your own projects to save time and make your code more efficient. Happy coding!