---
title:    "Elixir recipe: Using regular expressions"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are powerful tools for pattern matching and string manipulation in many programming languages, including Elixir. They allow you to refine your search criteria and perform complex string operations with just a few lines of code, making them a valuable skill for any programmer to have in their toolkit.

## How To

Using regular expressions in Elixir is straightforward and follows a similar syntax to other languages. Let's take a look at an example of using a regular expression to match a URL.

```Elixir
url = "www.example.com/path/to/resource"
regex = ~r/(https?:\/\/)?(www\.)?([a-z]+\.[a-z]+)(\/.*)?/
matches = Regex.run(regex, url)
IO.inspect matches
```

Here, we have a URL stored in a variable called `url` and a regular expression stored in a variable called `regex`. Notice the `~r` prefix, which indicates that we are using a regular expression. Next, we use the `run` function from the `Regex` module to match the regular expression against the URL. Finally, we use the `IO.inspect` function to print out the matches.

The output of the above code will be:

```
["www.example.com/path/to/resource", nil, "www.", "example.com", "/path/to/resource"]
```

This array contains all of the matches from our regular expression. The first item is the full URL, while the subsequent items are the captured groups from our regex.

## Deep Dive

Regular expressions in Elixir have many features and modifiers that allow you to customize your search criteria. Here are a few things to keep in mind:

- The `~r` prefix can be used with the `sigil_r` notation for inline regular expressions, making it easier to write and read complex regexes.
- Elixir also has a `Regex.replace` function for replacing portions of a string that match a given regular expression. This can be useful for tasks like sanitizing user input.
- The `Regex.match?` function returns a boolean indicating whether or not a match was found, instead of returning an array of matches like the `Regex.run` function.
- Elixir's regular expressions are case-sensitive by default, but this can be changed by using the `i` modifier at the end of the regex, such as `~r/regex/i`.

See the [Elixir docs](https://hexdocs.pm/elixir/Regex.html) for more information and examples on using regular expressions in Elixir.

## See Also

- [Regular Expressions 101](https://regex101.com/)
- [Mastering Regular Expressions](https://regexone.com/)