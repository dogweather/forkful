---
title:    "Elixir recipe: Concatenating strings"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why

Concatenating strings is a fundamental skill in any programming language. It allows you to combine multiple strings into one, giving you the ability to create dynamic and customizable output in your programs.

## How To

To concatenate strings in Elixir, you can use the `<>` operator. This operator takes two strings as operands and combines them into one. Let's see an example:

```Elixir
name = "John"
greeting = "Hello, "
full_greeting = greeting <> name
```

The above code will result in `full_greeting` being `"Hello, John"`. You can also use the `<>` operator to concatenate more than two strings, such as:

```Elixir
first_name = "John"
last_name = "Smith"
full_name = first_name <> " " <> last_name
```

The result of `full_name` will be `"John Smith"`. You can also use variables within the `<>` operator, such as:

```Elixir
first_name = "John"
last_name = "Smith"
address = "123 Main Street"

order = first_name <> " " <> last_name <> " ordered from " <> address
```

The output of `order` will be `"John Smith ordered from 123 Main Street"`.

## Deep Dive

It's important to note that the `<>` operator converts all operands to strings before concatenating them. This means you can use it to concatenate not just strings, but also integers, floats, booleans, and any other data types that can be converted to strings.

Another approach to concatenating strings is by using the `String.concat/1` function, which takes a list of strings and concatenates them into one. Let's see an example:

```Elixir
name = "John"
greeting = "Hello, "
full_greeting = String.concat([greeting, name])
```

The result of `full_greeting` will be the same as before: `"Hello, John"`. However, using `String.concat/1` can be more useful when you have a dynamic number of strings to combine.

## See Also

- [Elixir String Module](https://hexdocs.pm/elixir/String.html)
- [Learn You Some Erlang - Strings](https://learnyousomeerlang.com/strings)
- [Elixir Documentation - String Module](https://elixir-lang.org/docs/stable/elixir/String.html)