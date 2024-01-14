---
title:    "Gleam recipe: Using regular expressions"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are a powerful tool that can greatly increase the efficiency of your programming tasks. They allow for precise pattern matching and manipulation of text, making tasks such as data parsing and text replacement much easier.

## How To

To use regular expressions in Gleam, you first need to import the `re` module. This can be done with the following code:

```Gleam
import re
```

Once imported, you can use the `match` function to search for a specific pattern within a string. For example, if you wanted to find all words that contain the letter "a" in a string, you could use the following code:

```Gleam
let sentence = "She sells seashells by the seashore"
let pattern = re.compile("a")

match(pattern, sentence)

// Output: ["sells", "seashells", "seashore"]
```

Additionally, you can use the `sub` function to replace certain patterns with other strings. For example, if you wanted to replace all occurrences of "sea" with "ocean" in the above string, you could use the following code:

```Gleam
let sentence = "She sells seashells by the seashore"
let pattern = re.compile("sea")

sub(pattern, "ocean", sentence)

// Output: "She sells oceanoceans by the oceanoceanore"
```

## Deep Dive

Regular expressions are not limited to simple pattern matching and replacement. They can also handle more complex operations such as grouping, capturing, and lookaround. These advanced features allow for even more precise manipulation of text.

For example, if you wanted to match a date in the format of "dd/mm/yyyy", you could use the following code:

```Gleam
let date = "Today's date is 16/02/2021"
let pattern = re.compile("(\d{2}/\d{2}/\d{4})")

match(pattern, date)

// Output: ["16/02/2021"]
```

The parentheses in the pattern create a capturing group, allowing you to access the specific date that was matched.

## See Also

To learn more about regular expressions in Gleam, check out the official documentation and additional resources below:

- [Gleam Documentation: re Module](https://gleam.run/articles/learn/regular-expressions.html#re-module)
- [Regular Expressions 101: Interactive Regex Tutorial](https://regex101.com/)
- [Python Documentation: Regular Expression Syntax](https://docs.python.org/3/library/re.html#regular-expression-syntax)

Happy coding with regular expressions in Gleam! 🚀