---
title:                "Elixir recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why
Substring extraction is a common operation in string manipulation that allows us to extract a portion of a string based on a given index or range. This can be useful in a variety of scenarios, such as data parsing, text processing, and string formatting.

## How To
In Elixir, there are several functions available for extracting substrings from a string. Let's take a look at a few examples using the built-in `String` module.

### Extracting a single character
To extract a single character from a string, we can use the `String.at/2` function. The first argument is the string, and the second argument is the index of the character we want to extract.

```Elixir
string = "Hello World"
String.at(string, 4)
```

The output for this example would be `o`, as it is the 4th character in the string.

### Extracting a range of characters
To extract a range of characters from a string, we can use the `String.slice/3` function. The first argument is the string, the second argument is the starting index, and the third argument is the ending index (exclusive).

```Elixir
string = "Hello World"
String.slice(string, 0, 5)
```

The output for this example would be `Hello`, as it starts at index 0 and ends at index 5 (excluding the character at index 5).

### Extracting a substring based on a pattern
To extract a substring based on a pattern, we can use the `String.split/2` function. The first argument is the string, and the second argument is a regular expression pattern.

```Elixir
string = "Elixir is awesome!"
String.split(string, ~r/\s+/)
```

The output for this example would be `["Elixir", "is", "awesome!"]`, as it splits the string at every whitespace.

## Deep Dive
Under the hood, these functions use a concept called "slicing" to extract substrings. Slicing involves splitting the original string into smaller chunks and returning the chunk that matches the given index or pattern. This makes it efficient and performant, especially when working with large strings.

In addition to the functions mentioned above, the `String` module also provides other functions for substring extraction, such as `String.first/2` and `String.last/2` for extracting the first and last character, `String.ends_with?/2` and `String.starts_with?/2` for checking if a string starts or ends with a given substring, and `String.trim/2` for removing leading and trailing whitespaces.

## See Also
- [Elixir String module documentation](https://hexdocs.pm/elixir/String.html)
- [Elixir pattern matching tutorial](https://elixirschool.com/en/lessons/basics/pattern-matching/)
- [Introduction to Elixir strings article by Pluralsight](https://www.pluralsight.com/guides/introduction-to-elixir-strings)