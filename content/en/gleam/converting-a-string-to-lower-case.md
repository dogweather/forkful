---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case means turning all the uppercase letters in a piece of text to lowercase. Programmers do this to standardize data for comparison or processing in a case-insensitive manner.

## How to:

A string in Gleam can be converted to lower case using the `to_lower` function from the `gleam/string` module. 

```Gleam
import gleam/string.{to_lower}


pub fn main(args: List(String)) {
  let my_string = "HELLO GLEAM"
  let lower_string = my_string |> to_lower
  io.println(lower_string)
}
```

When running this code, you'll see the `HELLO GLEAM` string turned into `hello gleam` on your terminal.

## Deep Dive

Historically, the need for different casing (lowercase and uppercase) emanated from the era of mechanical typewriters to provide emphasis and clarity in written communication. In modern times, with the advent of computer programming, different casing has found new uses like differentiating constants (uppercase) and variables (lowercase) in some languages.

While the `to_lower` function from the `gleam/string` module is currently the most straightforward and built-in way to convert a string to lower case in Gleam, certain programming languages provide alternatives. Among these, employing ASCII values to change casing, using locale-specific conversions, or even leveraging regular expressions.

As for its implementation in Gleam, the `to_lower` function uses Rust's underlying `to_lowercase` method, which in turn uses Unicode's `SpecialCasing.txt` and `CaseFolding.txt` properties for correct letter transformation involving multi-character results and locale-specific changes, respectively.

## See Also

For reference about other built-in Gleam string methods, navigate to [Gleam String Module Documentation](https://hexdocs.pm/gleam_stdlib/gleam/string/).

To understand more about the Unicode used for the lowercase conversion, see the [Unicode's Special Casing Documentation](http://www.unicode.org/Public/UNIDATA/SpecialCasing.txt) and [Case Folding Documentation](http://www.unicode.org/Public/UNIDATA/CaseFolding.txt).