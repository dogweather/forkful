---
title:                "Gleam recipe: Searching and replacing text"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Searching and replacing text is a common task in programming that allows you to quickly and efficiently update and modify code. Whether you need to fix a typo, change a variable name, or update a URL, using the Gleam language for searching and replacing text can save you time and effort.

## How To
To search and replace text in Gleam, you can use the built-in `String.replace` function. This function takes in two arguments: the string to search for and the string to replace it with.

```Gleam
let original_str = "Hello World"
let new_str = String.replace("World", "Universe", original_str)
```

In this example, the string "Hello World" is searched for the word "World" and replaced with "Universe". The output of `new_str` would be "Hello Universe".

If you want to replace all instances of a word or phrase, you can use the `String.replace_all` function instead. This function takes in the same arguments as `String.replace` but replaces all occurrences instead of just the first one.

```Gleam
let original_str = "I love pizza, pizza is my favorite food"
let new_str = String.replace_all("pizza", "sushi", original_str)
```

The output of `new_str` would be "I love sushi, sushi is my favorite food" as all instances of "pizza" have been replaced with "sushi".

## Deep Dive
In addition to the basic `String.replace` and `String.replace_all` functions, there are advanced functions in Gleam that allow for more complex searching and replacing.

For example, the `String.replace_regex` function allows you to use regular expressions to specify what to search for and replace. This can be useful for scenarios where there is not a specific string to search for, but rather a pattern.

```Gleam
let original_str = "Today's date is 07-12-2021"
let new_str = String.replace_regex("[:digit:]+", "12", original_str)
```

The output of `new_str` would be "Today's date is 07-12-2021" as the regular expression has replaced the numbers with "12".

Additionally, Gleam also has the `String.replace_range` function which allows you to specify a specific range within the string to search and replace. This can be useful if you only want to replace a portion of a string and not the entire thing.

```Gleam
let original_str = "Hello world"
let new_str = String.replace_range(6, 10, "Gleam", original_str)
```

The output of `new_str` would be "Hello Gleam" as the range of characters 6 to 10 (inclusive) have been replaced with "Gleam".

## See Also
- [Gleam language documentation](https://gleam.run/book)
- [Regular expressions in Gleam](https://gleam.run/examples/regular-expressions)
- [String functions in Gleam](https://gleam.run/libraries/stdlib/String.html)