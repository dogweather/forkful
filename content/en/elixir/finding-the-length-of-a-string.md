---
title:    "Elixir recipe: Finding the length of a string"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why

When working with strings in Elixir, one common task is finding the length of a string. This is a crucial operation, especially when validating input or manipulating strings in some way. By knowing how to find the length of a string, you can enhance your Elixir programming skills and improve the quality of your code.

## How To

In Elixir, the `len()` function can be used to find the length of a string. Let's take a look at how it works in practice:

```Elixir
str = "Hello, world!"
len(str)
```

The above code will output `13`, as there are 13 characters in the string.

If you want to get the length of a string as a number instead of just the number of characters, you can use the `byte_size()` function. This is useful if you need to limit the number of bytes in a string, for example:

```Elixir
str = "Hello, world!"
byte_size(str)
```

The above code will output `15`, as each character in the string takes up one byte of memory.

Strings can also contain non-alphanumeric characters, such as emojis or accented letters. In these cases, the `length()` function can be used to get the number of Unicode code points in the string:

```Elixir
str = "Hello, 😊!"
length(str)
```

The above code will output `6`, as the smiley face emoji takes up two code points.

## Deep Dive

In Elixir, strings are represented as a special kind of binary called a UTF-8 encoded binary. This means that each character in a string is represented by a sequence of bytes, depending on the number of code points and the type of character. For example, emojis take up more code points and therefore require more bytes.

Knowing this, the `len()` function calculates the length of a string by counting the number of bytes in the binary. On the other hand, the `byte_size()` function simply returns the number of bytes in the binary, regardless of how many code points it contains.

It's also worth noting that since Elixir strings are immutable, finding the length of a string is an O(n) operation, where n is the number of bytes in the string. This means that the time it takes to find the length increases as the string gets longer.

## See Also

- [Elixir String module documentation](https://hexdocs.pm/elixir/String.html)
- [Elixir data types](https://elixir-lang.org/getting-started/basic-types.html)
- [Unicode code points](https://unicode.org/charts/)