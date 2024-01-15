---
title:                "Using regular expressions"
html_title:           "Gleam recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are a powerful tool that helps programmers handle strings in a more efficient and flexible way. With regular expressions, you can easily search, match, and manipulate text, making it a useful skill for any developer.

## How To

```Gleam
let text = "Hello, world!"
let matched = match Regex.new("world").find(text) {
  |Ok(matched) -> matched
  | _ -> "Not found"
}

gleam.io.print(matched) // "world"
```

Regular expressions in Gleam are created using the `Regex.new()` function, which takes in a pattern as its argument. The regex pattern can include special characters and modifiers to define what text to search for. 

In the example above, we first create a string variable `text` with the value "Hello, world!". Then, we use the `Regex.find()` function to search for the word "world" within the text. If it is found, the `matched` variable will be assigned the value of the matched text. If not, it will default to "Not found". Finally, we use the `gleam.io.print()` function to print out the matched text.

Regular expressions in Gleam are also equipped with modifiers that allow for more specific searches. For example, the `i` modifier can be used to make the search case-insensitive. Here's an example:

```Gleam
let text = "I love coding!"
let matched = match Regex.new("coding").find(text, Regex.i) {
  |Ok(matched) -> matched
  | _ -> "Not found"
}

gleam.io.print(matched) // "coding"
```

## Deep Dive

Regular expressions in Gleam follow the Perl 5 syntax, making it easy for those familiar with Perl or other languages that use this syntax. Gleam also has a helpful `Regex` module with functions such as `.find()` and `.replace()` to make working with regex more seamless.

One thing to note is that since Gleam is a functional language, regex patterns are immutable once created. This means that if you need to modify a regex pattern, you will have to create a new regex with the updated pattern.

## See Also

- [Gleam Documentation: Regular Expressions](https://gleam.run/documentation/learn/regular-expressions)
- [Regular Expressions 101: A great tool for testing and learning regex](https://regex101.com/)
- [Mastering Regular Expressions by Jeffrey Friedl - A comprehensive guide to regular expressions](http://regex.info/book.html)