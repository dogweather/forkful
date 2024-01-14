---
title:    "Elixir recipe: Concatenating strings"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

When programming in Elixir, you may come across the need to join two or more strings together. This is known as concatenation, and it is a common operation that can come in handy in various scenarios. By learning how to concatenate strings in Elixir, you can enhance your ability to manipulate data and create more complex programs.

## How To

To join strings together in Elixir, you can use the `<>` operator. This operator takes two strings and returns a new string containing the combined text. Let's look at an example:

```
Elixir
first_name = "John"
last_name = "Doe"

full_name = first_name <> " " <> last_name

IO.puts full_name
```

This code will output `John Doe`, combining the first and last names into one string. You can also use the `<>` operator to join more than two strings together. For instance, `first_name <> " " <> middle_name <> " " <> last_name` will return `John Adam Doe` for a person with the middle name "Adam".

Another way to concatenate strings in Elixir is by using the `<>` function. This function takes a list of strings and combines them into one string. Here's an example:

```
Elixir
names = ["John", "Adam", "Doe"]

full_name = Enum.join(names, " ")

IO.puts full_name
```

This will also output `John Adam Doe`, just like in the previous example.

## Deep Dive

When concatenating strings, it is important to note that the `<>` operator and `<>` function both create brand new strings instead of modifying the existing ones. This is due to Elixir's immutability, which means that variables cannot be changed after they are assigned a value.

You can also use the `<<>>` binary syntax for string concatenation. This syntax operates similarly to the `<>` operator, but it is more efficient when dealing with larger strings. Here's an example:

```
Elixir
greeting = "Hello"
name = "John Doe"

message = <<greeting::binary-size(5), " ", name::binary>>

IO.puts message
```

This code will output `Hello John Doe` using the `<<>>` binary syntax. The `binary-size(5)` specifies the size of the greeting string, optimizing its memory usage.

## See Also

To learn more about strings in Elixir, check out these resources:

- [Elixir Strings Documentation](https://hexdocs.pm/elixir/String.html)
- [Elixir Enum Module](https://hexdocs.pm/elixir/Enum.html)
- [Binary Syntax in Elixir](https://hexdocs.pm/elixir/master/binaries.html)