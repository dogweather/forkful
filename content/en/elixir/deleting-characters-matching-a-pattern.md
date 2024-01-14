---
title:    "Elixir recipe: Deleting characters matching a pattern"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why

When working with strings, it is sometimes necessary to remove certain characters that match a specific pattern. This can be useful for data manipulation, input validation, or formatting purposes. In Elixir, there are several ways to achieve this, and it is a useful skill for any developer to have in their arsenal.

## How To

To delete characters matching a pattern in Elixir, we can use the `String.replace` function. This function takes in three parameters: the original string, the pattern to be matched, and the replacement string. The pattern can be a regular expression or a string. Let's take a look at some examples to better understand how to use this function.

```
# Using a string as the pattern
iex> String.replace("Hello World", "o", "")
"Hell Wrld"

# Using a regular expression as the pattern
iex> String.replace("0123456789", ~r/[24680]/, "")
"13579"
```

In the first example, we are replacing all instances of the letter "o" with an empty string, effectively removing it. In the second example, we are using a regular expression to replace all even digits with an empty string.

It is also important to note that `String.replace` is a Unicode-aware function, meaning it can handle characters from different languages and alphabets.

## Deep Dive

The `String.replace` function uses the regular expression engine provided by Erlang's `:re` module. This gives us access to powerful regular expression features such as capturing groups and non-greedy matches. This makes it easy to manipulate strings in complex ways.

Additionally, `String.replace` is a pure function, meaning it does not modify the original string but instead returns a new string with the changes applied. This is in line with Elixir's philosophy of immutability and makes our code safer and easier to reason about.

## See Also

- [String.replace documentation](https://hexdocs.pm/elixir/String.html#replace/3)
- [Erlang :re module documentation](http://erlang.org/doc/man/re.html)
- [Mastering Regular Expressions by Jeffrey E.F. Friedl](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)

By mastering the `String.replace` function, you'll have a powerful tool at your disposal for manipulating strings in Elixir. Keep exploring and experimenting with regular expressions to take your skills to the next level. Happy coding!