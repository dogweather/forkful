---
title:                "Перетворення рядка на великі літери"
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що і чому?)
Capitalizing a string means making its first character uppercase and the rest lowercase. It's often done to standardize the formatting of user input for names, titles, or to begin sentences properly.

## How to: (Як це зробити:)
In Gleam, you'll write a function to capitalize strings. Check this out:

```gleam
pub fn capitalize(text: String) -> String {
  let first = text
    |> String.slice(0, 1)
    |> String.uppercase()
  
  let rest = text
    |> String.slice(1, text.len())
    |> String.lowercase()
  
  first + rest
}

pub fn main() {
  let message = "hello, world!"
  capitalize(message)
  |> io.println
}
```

If you run this, your output will look like this:

```
Hello, world!
```

## Deep Dive (Поглиблений аналіз)
Before programming, capitalization rules were set by language experts and typesetters. In the digital age, we mimic those rules in code. We could also use libraries or external tools but writing a custom function gives us control and understanding. Under the hood, Gleam and most programming languages define string manipulation functions that respect Unicode, which is essential for proper capitalization, especially with non-ASCII characters.

Some languages provide built-in methods; for instance, Python has `.capitalize()`. Gleam doesn't have a built-in capitalization function – hence our custom one.

## See Also (Додатково)
To go further into string manipulation in Gleam, check out the official documentation:


And for general understanding of Unicode and character properties:

- The Unicode Consortium: [https://unicode.org](https://unicode.org)
