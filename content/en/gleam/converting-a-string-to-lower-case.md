---
title:                "Converting a string to lower case"
html_title:           "Gleam recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Gleam: The Efficient Way to Convert Strings to Lower Case

## What & Why?

Converting a string to lower case is a common task in programming that involves changing all the characters in a string to their lower case equivalents. This is useful for various reasons, such as improving readability, making data more consistent, and comparing strings without worrying about case sensitivity. Most programming languages have built-in functions for this, and Gleam is no exception.

## How to:

Converting a string to lower case in Gleam is a simple and straightforward process. You can use the ```String.to_lower_case()``` function, which takes a string as an argument and returns a new string with all lower case characters. Let's look at an example:

```
Gleam> let my_string = "Gleam is Awesome"
Gleam> String.to_lower_case(my_string)
"gleam is awesome"
```

As you can see, all the characters have been converted to lower case, making the string easier to read and manipulate. You can also directly use the ```to_lower_case()``` method on any string variable, like this:

```
Gleam> let my_string = "Gleam is Awesome"
Gleam> my_string.to_lower_case()
"gleam is awesome"
```

It's that simple!

## Deep Dive:

Historically, converting strings to lower case has been a challenging task for developers due to the varying case handling among different languages. Gleam handles this issue by using the Unicode standard, which ensures consistency in case handling across different languages. Some alternative methods for converting strings to lower case include using regular expressions or creating a custom function that iterates over each character. However, these methods may not be as efficient or reliable as using Gleam's built-in function.

## See Also:

- [Gleam documentation on string functions](https://gleam.run/articles/strings)
- [Unicode case mapping](https://unicode.org/charts/collation/)
- [Gleam's GitHub repository](https://github.com/gleam-lang/gleam)