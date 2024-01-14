---
title:                "Gleam recipe: Deleting characters matching a pattern"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why
Deleting characters matching a pattern is a common task in programming that can help streamline processes and remove unwanted or unnecessary data. By removing characters that follow a specific pattern, code can be cleaner, more efficient, and easier to understand.

## How To
To delete characters matching a pattern in Gleam, you can use the `String.replace` function. This function takes in three arguments: the original string, the pattern to match, and the replacement string.

```
Gleam

let original_string = "Hello, World!"
let modified_string = String.replace(original_string, ",", "")

# Output: "Hello World!"
```

In the above example, we use the `String.replace` function to remove the comma from the original string. The third argument is an empty string, indicating that we want to delete the matched pattern.

You can also use regular expressions in the pattern argument to match more complex patterns. For example, to remove all numbers from a string, you can use the regex `#[0-9]+#` to match any number and replace it with an empty string.

```
Gleam

let original_string = "I have 5 apples and 3 bananas"
let modified_string = String.replace(original_string, "#[0-9]+#", "")

# Output: "I have apples and bananas"
```

It's important to note that `String.replace` only replaces the first matched pattern. To replace all instances, you can use `String.replace_all`.

```
Gleam

let original_string = "I really love love love programming"
let modified_string = String.replace_all(original_string, "love", "enjoy")

# Output: "I really enjoy enjoy enjoy programming"
```

## Deep Dive
Under the hood, `String.replace` and `String.replace_all` use the `String.replace_range` function, which takes in the index range to replace. This allows for more customization, as you can specify exactly which characters you want to replace.

Additionally, Gleam also offers the `String.filter` function, which allows you to delete characters based on a condition. This can be useful for removing specific characters, such as all non-alphabetic characters.

## See Also
- Official Gleam Documentation on `String.replace`: https://gleam.run/documentation/std/string.html#replace
- Regular Expressions Tutorial: https://www.regular-expressions.info/
- Gleam Standard Library `String` module: https://gleam.run/documentation/std/string.html