---
title:                "Interpolating a string"
html_title:           "Elixir recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Interpolating a string in Elixir simply means inserting data into a string. Instead of manually concatenating strings and variables, this method allows for easier and cleaner string manipulation. It is a common practice among programmers to make their code more readable and maintainable.

## How to:

To interpolate a string in Elixir, use the `#{...}` syntax within a double-quoted string. The expression within the brackets will be evaluated and the result will be inserted into the string. Let's look at an example:

```Elixir
name = "John"
age = 28
"Hello, my name is #{name} and I am #{age} years old."
```

The output would be: `Hello, my name is John and I am 28 years old.`

You can also use interpolation with function calls, as shown below:

```Elixir
"Today is #{Date.utc_today()}."
```

Output: `Today is 2019-11-14`.

## Deep Dive

Interpolating strings is a feature that originated from the Ruby programming language, which inspired the creators of Elixir. This helpful feature also exists in other popular languages such as JavaScript and Python.

An alternative to interpolation in Elixir is the `<>` concatenation operator. It works similarly but can become cumbersome for longer strings and multiple variables.

Internally, Elixir makes use of the `String.Chars` protocol to convert any data type into a string, making it possible to interpolate any type of data.

## See Also

To learn more about string interpolation in Elixir, check out the [official documentation](https://hexdocs.pm/elixir/String.html#interpolation) and [ElixirSchool](https://elixirschool.com/en/lessons/basics/string-interpolation/). You can also explore other useful features of Elixir such as pattern matching and concurrency. Happy coding!