---
title:                "Elixir recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is a crucial aspect of programming in any language, including Elixir. It allows us to combine multiple strings together to form a new, longer string. This can be useful for creating dynamic messages, building URLs, or formatting text in various ways. Learning how to effectively concatenate strings in Elixir can greatly enhance your coding skills and make your code more versatile.

## How To

To concatenate strings in Elixir, we can use the `<>` operator or the `string_concat()` function. Let's see some examples:

```elixir
# Using the <> operator
"Greetings " <> "from " <> "Elixir!" #=> "Greetings from Elixir!"

# Using the string_concat() function
string_concat("Hello ", "world!") #=> "Hello world!"
```

As we can see, both methods produce the same result. However, when dealing with longer strings or more complex concatenations, it is recommended to use the `string_concat()` function for better readability and maintainability.

We can also use interpolation to concatenate strings with variables:

```elixir
name = "Lisa"

# Using the <> operator
"Hello " <> name <> "! Welcome to Elixir!" #=> "Hello Lisa! Welcome to Elixir!"

# Using the string_concat() function
string_concat("Hello ", name, "! Welcome to Elixir!") #=> "Hello Lisa! Welcome to Elixir!"
```

Lastly, we can use the `string()` function to convert non-string values to strings before concatenating:

```elixir
"Today is " <> string(3) <> "rd of " <> string(5) #=> "Today is 3rd of 5"
```

## Deep Dive

Strings in Elixir are represented as binaries - a sequence of 8-bit bytes. This allows for efficient handling of strings, as binary operations are faster than character-based operations.

Elixir also provides the `IO.puts()` function for printing strings to the console. It automatically adds a new line at the end of the output, making it perfect for printing concatenated strings:

```elixir
IO.puts("Hi there!")
IO.puts("What's " <> "your " <> "name?") #=> Hi there!
#=> What's your name?
```

Additionally, we can also use `IO.inspect()` to output the string representation of any data structure, including concatenated strings:

```elixir
IO.inspect("Good" <> " " <> "morning!") #=> "Good morning!"
```

## See Also

- [Elixir String module](https://hexdocs.pm/elixir/String.html)
- [Elixir String concatenation guide](https://elixirschool.com/lessons/basics/strings/#string-concatenation)
- [Elixir String interpolation](https://elixir-lang.org/getting-started/strings-and-binaries.html#string-interpolation)