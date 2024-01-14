---
title:                "Elixir recipe: Using regular expressions"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, also known as regex, are an essential tool in any programmer's toolkit. They allow us to search for and manipulate text in a precise and efficient manner. In the world of Elixir, regex is particularly powerful due to its integration with pattern matching.

## How To

To use regex in Elixir, we first need to import the `Regex` module. We can then use the `=~` operator to match a string against a regular expression:

```Elixir
import Regex

"Hello world" =~ ~r/world/
#=> true
```

In this example, we searched for the word "world" in the string "Hello world" and found a match. The `=~` operator returns `true` if there is a match and `false` if there isn't. 

We can also use regex to extract specific parts of the string using capture groups. For example, if we have a string with a phone number in the format `XXX-XXX-XXXX`, we can use regex to extract the area code:

```Elixir
phone_number = "555-123-4567"
number_match = phone_number =~ ~r/(\d{3})-\d{3}-\d{4}/

Regex.group(number_match, 1)
#=> "555"
```

In this case, we used `(\d{3})` as our capture group to extract the first three digits of the phone number. We can then use the `Regex.group` function to specify which group we want to return, in this case, the first group.

There are many more ways to use regex in Elixir, and the syntax may take some time to get used to. However, with regular practice and experimentation, you'll become a regex pro in no time.

## Deep Dive

Elixir's regex module provides many helpful functions for working with regular expressions. Some notable ones include `Regex.replace`, which allows us to replace parts of a string that match a regex pattern, and `Regex.scan`, which returns all matches in a string.

One crucial concept to understand when working with regex in Elixir is greedy vs. non-greedy matching. Greedy matching means that the regex pattern will match as much as possible, while non-greedy matching will stop at the first match. For example, in the string "Hello <b>world</b>", the regex pattern `<.*>` would match the entire string because it is greedy, while the pattern `<.*?>` would only match `<b>` because it is non-greedy.

For a more in-depth explanation of different regex concepts and how to use them in Elixir, check out the official documentation or online tutorials.

## See Also

- [Elixir Regular Expressions Documentation](https://hexdocs.pm/elixir/Regex.html)
- [Regular Expressions Tutorial - Learn to Use Regex](https://www.regular-expressions.info/tutorial.html)
- [Mastering Regular Expressions - Book by Jeffrey E.F. Friedl](https://www.oreilly.com/library/view/mastering-regular-expressions/9780596528126/)