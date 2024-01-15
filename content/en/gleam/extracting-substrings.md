---
title:                "Extracting substrings"
html_title:           "Gleam recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why
Substrings are smaller pieces of a larger string that can be used for various purposes such as data manipulation or validation. Extracting substrings allows users to programmatically retrieve specific parts of a string without having to manually search and manipulate the entire string.

## How To
To extract substrings in Gleam, we use the built-in function `String.slice()` which takes in three arguments: the main string, the starting index, and the ending index. Let's take a look at an example:

```Gleam
let main_string = "Hello World!"
let substring = String.slice(main_string, 6, 11)
```

In the above code, we create a string variable called `main_string` and assign it the value of "Hello World!". Then, we use the `String.slice()` function to extract the substring starting at index 6 and ending at index 11. The resulting output would be "World". 

We can also use negative numbers to represent the starting and ending indexes, which counts from the end of the string instead of the beginning. For example:

```Gleam
let main_string = "Gleam is a functional language"
let substring = String.slice(main_string, -8, -1)
```

In this case, the extracted substring would be "language".

## Deep Dive
In Gleam, the `String.slice()` function creates a new string rather than modifying the original one. This means that the original main string remains unchanged. It also supports inclusive and exclusive indexes, where the starting index is included in the substring but the ending index is not. For example:

```Gleam
let main_string = "Gleam is a functional language"
let inclusive_substring = String.slice(main_string, 0, 4)
// Output: "Glea"

let exclusive_substring = String.slice(main_string, 0, 5)
// Output: "Gleam"
```

Additionally, the ending index can be left empty, in which case the substring will be extracted from the starting index to the end of the string.

```Gleam
let main_string = "Gleam is a functional language"
let substring = String.slice(main_string, 14,)
// Output: "functional language"
```

## See Also
- Official documentation for `String.slice()` function: https://gleam.run/modules/stdlib/String.html#slice
- More examples of string manipulation in Gleam: https://gleam.run/dontpanic/strings.html