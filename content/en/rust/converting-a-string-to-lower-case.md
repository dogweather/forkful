---
title:                "Rust recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
Rust is a powerful programming language that provides developers with the ability to write safe and efficient code. One of the many useful features of Rust is its built-in string manipulation capabilities, including converting a string to lower case. This can be beneficial for data cleaning or standardizing input before processing it further.

## How To
To convert a string to lower case in Rust, we can use the `to_lowercase()` method from the `String` type. Let's take a look at an example:

```Rust
let my_string = "RUST PROGRAMMING"; // create a string variable
let lower_case_string = my_string.to_lowercase(); // call to_lowercase() method on the string
println!("{}", lower_case_string); // print the lower case string
```

The output of this code would be:

```Rust
rust programming
```

It's important to note that the `to_lowercase()` method returns a new string with the lower case version, and does not modify the original string. This ensures the immutability of strings in Rust.

## Deep Dive
Behind the scenes, the `to_lowercase()` method is implemented using Unicode's comprehensive database of character mappings. This allows for accurate conversions for characters in different languages and scripts. It also takes into account special cases such as the German Eszett (ß) being converted to "ss" instead of "ß" in lower case.

Additionally, the `to_lowercase()` method uses the current locale of the system, which means it will follow the rules of the language used in that locale. This allows for consistent and expected behavior, especially when working with multiple languages in the same codebase.

## See Also
For more information on string manipulation in Rust, check out these resources:
- [Rust Standard Library documentation on Strings](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rustonomicon - String Conversions](https://doc.rust-lang.org/nomicon/string.html#string-conversions)
- [Rust by Example - Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)

Happy coding!