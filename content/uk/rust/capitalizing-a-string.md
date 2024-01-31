---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Capitalizing a string means transforming the first letter of each word to uppercase. Programmers do this for formatting names, titles, or any text where a proper noun needs distinction.

## How to: (Як це зробити:)
```Rust
fn capitalize_first_letter(s: &str) -> String {
    s.split_whitespace()
        .map(|word| {
            word.chars()
                .enumerate()
                .map(|(i, c)| {
                    if i == 0 { c.to_uppercase().to_string() } else { c.to_string() }
                })
                .collect::<String>()
        })
        .collect::<Vec<String>>()
        .join(" ")
}

fn main() {
    let input = "rust programming language";
    let capitalized = capitalize_first_letter(&input);
    println!("{}", capitalized); // Output: Rust Programming Language
}
```

## Deep Dive (Поглиблений Аналіз)
Capitalizing strings is no new concept; it's been a common operation since early text-processing and word-processing programs. In Rust, there isn't a built-in method to capitalize every word in a string directly, so a custom function like `capitalize_first_letter` is crafted. Rust's `.map()` and `.enumerate()` iterators are used here to iterate over each word and then each character, respectively. The `to_uppercase()` method applies only to the first character, providing capitalization.

Alternative methods like regular expressions or other crates could also achieve this, but they come with performance trade-offs and added complexity. Rust prioritizes safety and speed, so the standard library's methods are often preferred.

## See Also (Дивіться Також)
- [Rust std::str Documentation](https://doc.rust-lang.org/stable/std/str/)
- [The Rust Programming Language Book](https://doc.rust-lang.org/book/)
- [Rust By Example](https://doc.rust-lang.org/rust-by-example/)
- [crates.io, for community-contributed Rust tools](https://crates.io/)
