---
title:                "Gleam recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Searching and replacing text is a fundamental task in programming, allowing developers to efficiently make changes to large amounts of code. Gleam, with its robust string manipulation capabilities, makes this task even easier and smoother. In this blog post, we will dive into the basics of searching and replacing text in Gleam.

## How To
To search and replace text in Gleam, we can use the `replace` function from the `gleam/string` module. This function takes in three arguments - the string to be searched, the old string to be replaced, and the new string to replace it with.

```
Gleam REPL
---
import gleam/string

let original_string = "Hello, World!"
let new_string = replace(original_string, "Hello", "Hey")

new_string # Hey, World!
```

We can also use regular expressions as the old string to perform more complex and dynamic replacements. The `regex` function from the `gleam/regex` module can be used to create a regex pattern, which can then be passed in as the old string.

```
Gleam REPL
---
import gleam/string

let original_string = "This is a sample string with numbers 1234."
let pattern = regex("[0-9]+")
let new_string = replace(original_string, pattern, "")

new_string # This is a sample string with numbers .
```

## Deep Dive
While the `replace` function is useful for basic searching and replacing, it also has some additional options that can be explored for more advanced use cases. One such option is the `count` argument, which allows us to specify the number of replacements to be made.

```
Gleam REPL
---
import gleam/string

let original_string = "One fish, two fish, red fish, blue fish."
let new_string = replace(original_string, "fish", "rabbit", count=2)

new_string # One rabbit, two rabbit, red fish, blue fish.
```

There is also the `case_sensitive` argument, which can be set to `false` to perform a case-insensitive search and replace.

Additionally, the `replace` function supports unicode characters, making it suitable for internationalization and multilingual applications.

## See Also
- Gleam's official documentation on string manipulation: http://gleam.run/book/std-libraries/strings.html
- Regex tutorial for beginners: https://www.regular-expressions.info/tutorial.html