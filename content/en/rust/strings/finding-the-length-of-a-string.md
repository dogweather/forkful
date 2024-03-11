---
date: 2024-01-20 17:48:16.602904-07:00
description: "Finding the length of a string means counting the number of characters\
  \ it contains. Programmers do this to validate, format, or process text data\u2026"
lastmod: '2024-03-11T00:14:33.748753-06:00'
model: gpt-4-1106-preview
summary: "Finding the length of a string means counting the number of characters it\
  \ contains. Programmers do this to validate, format, or process text data\u2026"
title: Finding the length of a string
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string means counting the number of characters it contains. Programmers do this to validate, format, or process text data efficiently.

## How to:
Rust gives you `len()` for straight-up length:

```Rust
fn main() {
    let greeting = "Hello, world!";
    println!("Length: {}", greeting.len());
}
```

Output: `Length: 13`

But beware, `len()` counts bytes, not characters. For character count, use `.chars().count()`:

```Rust
fn main() {
    let greeting = "¡Hola, mundo!";
    println!("Character count: {}", greeting.chars().count());
}
```

Output: `Character count: 12`

## Deep Dive
`len()` counts bytes because Rust strings are UTF-8 encoded. Historically, early computers used ASCII, representing each character with a single byte. UTF-8, however, supports a vast array of characters, using 1 to 4 bytes each. 

When you call `len()`, Rust counts the bytes in a string, which is fast but won't always match the character count. For instance, emojis or certain accented characters take more than one byte. That's why `.chars().count()` matters—it iterates over the characters and gives the Unicode scalar value count, which is the actual character count most people expect.

As for alternatives, `.chars().count()` is accurate but slow for long strings because it has to iterate through each character. If performance is critical, and you're sure about dealing with ASCII or fixed-width Unicode characters, `len()` is more efficient.

Lastly, remember Rust's string indexing doesn't allow direct access by character position because of how UTF-8 encoding works. Rust prevents operations that could accidentally break or slice strings at invalid points, which might not represent full characters.

## See Also
- Rust's official string documentation: [https://doc.rust-lang.org/std/string/](https://doc.rust-lang.org/std/string/)
- The Rust Book on strings: [https://doc.rust-lang.org/book/ch08-02-strings.html](https://doc.rust-lang.org/book/ch08-02-strings.html)
- To understand UTF-8 vs ASCII further, check out [https://tools.ietf.org/html/rfc3629](https://tools.ietf.org/html/rfc3629)
