---
title:                "Searching and replacing text"
aliases:
- /en/rust/searching-and-replacing-text/
date:                  2024-01-20T17:58:41.958690-07:00
model:                 gpt-4-1106-preview
simple_title:         "Searching and replacing text"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text is the process of finding strings within strings and swapping them out for something else. Programmers do this to edit data, refactor code, or automate text manipulation.

## How to:

```Rust
fn main() {
    let text = "Hello there!";
    let updated_text = text.replace("there", "world");
    println!("{}", updated_text); // Prints "Hello world!"
}
```

Sample output:
```
Hello world!
```

## Deep Dive
Searching and replacing text has been around since early text editors emerged. Tools like sed in Unix made batch text processing common practice.

Rust takes an efficient, safe approach. The `replace` method, from the standard library's `str` type, is straightforward and checks at compile-time.

Alternatives to `replace` include regex for complex patterns or iterating characters to customize replacement logic.

Under the hood, `replace` in Rust creates a new `String`, iterates through the original, finds matches, and then constructs the new string with replacements. It handles Unicode well, which isn't trivial.

## See Also
- Rust's documentation on `replace`: https://doc.rust-lang.org/std/primitive.str.html#method.replace
- Regex crate for more complex use-cases: https://crates.io/crates/regex
- Sed's manual for historical reference: https://www.gnu.org/software/sed/manual/sed.html
