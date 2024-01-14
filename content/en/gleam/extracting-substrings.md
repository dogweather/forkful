---
title:    "Gleam recipe: Extracting substrings"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

Have you ever come across a long string of text and needed to extract specific parts of it? Maybe you needed just the first few characters or a specific section in the middle. Extracting substrings can save you time and effort by allowing you to isolate the desired information without having to manipulate the entire string.

## How To

To extract substrings in Gleam, we will use the `string` module's `slice` function. This function takes in a string, a starting index, and an ending index, and returns the substring between those indices.

```
Gleam code
import string

let text = "Hello, world!"

let first = string.slice(text, 0, 5)
// output: Hello

let middle = string.slice(text, 7, 12)
// output: world
```

We can also use negative indices to count from the end of the string. For example, using `-1` as the ending index will return the last character of the string.

```
Gleam code
let last = string.slice(text, -1, -4)
// output: ld!
```

The `slice` function also allows for partial ranges. If only the starting index is provided, it will return the substring from that index till the end of the string.

```
Gleam code
let end = string.slice(text, 7)
// output: world!
```

## Deep Dive

It's important to note that the `slice` function is inclusive of the starting index but exclusive of the ending index. This means that the character at the ending index will not be included in the substring.

We can also use variables or functions as the indices for more dynamic substring extraction. For example, we can create a function that takes in a string and returns a substring of the first three characters.

```
Gleam code
import string

fn first_three_chars(text) {
  string.slice(text, 0, 3)
}

let text = "Gleam is a powerful language!"

let result = first_three_chars(text)
// output: Gle
```

## See Also
- [Gleam string module documentation](https://gleam.run/documentation/std-lib/string/)
- [Gleam slicing tutorial](https://gleam.run/tutorials/strings/#slicing)

Happy coding!