---
title:                "Capitalizing a string"
html_title:           "Gleam recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means increasing the case of the first character of each word within the string. Programmers do this to properify names, titles, and more, enhancing user readability.

## How to:

Gleam programming is a breeze. For capitalizing strings, we use the `string.capitalize` function. Check out the code below:

```Gleam
import gleam/string

fn main() {
  let sentence = "hello, gleam world"
  let result = string.capitalize(sentence)
  IO.println(result)
}
```
Run the code, watch your console, and the output will be:
```Gleam
"Hello, Gleam World"
```
As simple as that!

## Deep Dive

A deeper dive into string capitalization discloses a few more facets. 

1. Historical context: Adopted from linguistic norms, capitalizing the first letter of each word in programming language has its roots from typesetting practices of the 17th century.

2. Alternatives: In Gleam, the `string.capitalize` function is your best bet for capitalizing string. It's straightforward, with no viable or simpler alternatives.

3. Implementation details: This function traverses the string, converting the first character of every word to uppercase while making the remaining characters lowercase.

## See Also:

For a better grasp, check out these useful sources.

1. [Gleam String documentation](https://gleam.run/book/tour/strings.html) : An all-inclusive guide on string manipulation in Gleam.
2. [Case conversion in various languages](https://en.wikipedia.org/wiki/Capitalization): To understand how different languages handle string capitalization.