---
title:    "Elixir recipe: Using regular expressions"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why

Regular expressions, also known as regex, are a powerful tool for manipulating and extracting data from strings. They allow us to search for specific patterns within a larger text and perform actions based on those patterns. This makes them extremely useful for tasks such as data validation, data cleaning, and text parsing.

## How To

To use regular expressions in Elixir, we first need to make use of the `Regex` module. This can be done by adding `use Regex` at the top of our Elixir file or simply by calling `Regex` functions with the `Regex.` prefix.

One of the most common uses of regular expressions is to check if a string matches a certain pattern. Let's say we want to check if a given string contains only numbers and no other characters. We can do this using the `match?` function from the `Regex` module. Here's an example of how we can use regular expressions to achieve this in Elixir:

```
Elixir
input = "123456"
Regex.match?(~r/\A\d+\z/, input) #=> true
input = "1234abc"
Regex.match?(~r/\A\d+\z/, input) #=> false
```

In the code above, we use the `~r` sigil to define our regular expression. The `\A` denotes the start of the string, `\d+` represents one or more digits, and `\z` denotes the end of the string. By using these special characters and metacharacters, we can create powerful patterns to match against our strings.

Apart from matching, we can also use regular expressions to extract specific parts of a string. This is done by using capturing groups `()` in our regular expressions. Let's use an example to better understand this:

```
Elixir
input = "John Doe"
Regex.run(~r/(\w+) (\w+)/, input) #=> ["John Doe", "John", "Doe"]
```

In the code above, we use capturing groups to extract the first and last name of the string. The matching string itself is returned as the first element of the resulting list, followed by the two captured groups.

## Deep Dive

Regular expressions can get complex when we start using more advanced metacharacters and quantifiers. Understanding how to use these special characters and their meanings is important for creating efficient and accurate regular expressions.

For example, the `*` quantifier means "zero or more", while the `+` quantifier means "one or more". This may seem similar, but it can lead to drastically different results when used in a regular expression. Another important metacharacter is the `?` which means "zero or one". It is often used to make certain parts of a regular expression optional.

In addition to these, there are many more metacharacters and quantifiers that can be used in combinations to create more complex regular expressions. Familiarizing yourself with these and their meanings will greatly enhance your ability to use regular expressions effectively in Elixir.

## See Also

- [Elixir Regex Module Documentation](https://hexdocs.pm/elixir/Regex.html)
- [Regular Expressions Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/elixir)
- [Mastering Regular Expressions](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/) (book)