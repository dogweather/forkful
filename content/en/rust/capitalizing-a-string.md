---
title:                "Rust recipe: Capitalizing a string"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to capitalize a string in Rust? Whether you're building a command-line tool or a web application, string manipulation is a common task in programming. In this blog post, we'll explore how to capitalize a string in Rust and why it might be useful.

## How To

Coding Example:

```Rust
let my_string = "hello world";
let capitalized_string = my_string.to_uppercase();
println!("Capitalized string: {}", capitalized_string); 
```

Output:

```
Capitalized string: HELLO WORLD
```

In Rust, we can use the `to_uppercase()` method to capitalize a string. This method returns a new string with all characters transformed to their uppercase equivalent. We can then print this new string or use it in further processing.

However, this method only works for ASCII strings. If you want to capitalize a string with non-ASCII characters, you can use the `to_uppercase()` method from the `unicase` crate. This method handles Unicode characters and supports various languages.

Coding Example:

```Rust
use unicase::UniCase;
let my_string = "héllò wórld";
let capitalized_string = UniCase::new(my_string).to_uppercase();
println!("Capitalized string: {}", capitalized_string); 
```

Output:

```
Capitalized string: HÉLLÒ WÓRLD
```

## Deep Dive

In Rust, strings are encoded in UTF-8 by default. This means that each character may occupy more than one byte in memory. When using the `to_uppercase()` method, Rust converts each byte to its uppercase equivalent, which may lead to unexpected results for non-ASCII characters.

To understand how Rust handles Unicode characters, it's helpful to know about the `char` type. In Rust, a `char` is not equivalent to a byte. Instead, it represents a Unicode Scalar Value, which can be composed of one or more bytes. This means that a `char` can represent both ASCII and non-ASCII characters.

When we use the `to_uppercase()` method, it iterates through each `char` in the string and converts it to its uppercase equivalent. This is why using this method with non-ASCII characters may result in incorrect output. By using the `to_uppercase()` method from the `unicase` crate, we can correctly handle Unicode characters.

## See Also

- Official Rust Documentation on Strings: https://doc.rust-lang.org/std/string/
- `to_uppercase()` method in the Rust Standard Library: https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase
- `to_uppercase()` method in the Unicase Crate: https://docs.rs/unicase/2.6.0/unicase/struct.CharIndices.html#method.to_uppercase