---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings means getting specific subsections of a full string of text. Programmers often use it to reduce large datasets into manageable pieces and to analyze or manipulate only the relevant bits of data.

## How to:

In Gleam, we have the `substring` function for extracting string subsets. It needs the start index, end index, and the parent string. Here's how you'd do it:

```Gleam
import gleam/string

let sentence = "Hello, World!"
let hello = string.slice(sentence, 0, 5)
let world = string.slice(sentence, 7, 12)
```
The output would be:

```Gleam
hello = "Hello"
world = "World"
```
Remember, Gleam uses zero-based indexing.

## Deep Dive

Substrings traces back to early computer science. It remains relevant today due its necessity in manipulating textual data. Indeed, various programming languages provide their substring functions. Gleam, though young, also offers it.

Alternatives to Gleam's `substring` method include using a regex function to match and pull out desired sections of a string. However, `string.slice` is simpler and more efficient for many common cases.

Behind the scenes, when you request a substring, the language creates a new string from the designated start and stop indexes. It doesn't modify the original string, as Gleam strings are immutable.

## See Also

For more about Gleam's string module, refer to the [Gleam string docs](https://hexdocs.pm/gleam_stdlib/gleam/string/). For examples of substring use cases and ways to work around in other languages, you can swing by [Rosettacode's substring page](http://rosettacode.org/wiki/Substring/Top_and_tail).