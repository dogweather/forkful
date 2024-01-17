---
title:                "Extracting substrings"
html_title:           "Rust recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings is the process of retrieving a smaller string from a larger string based on specific requirements, such as a starting and ending index, or a certain pattern. Programmers often use substring extraction to manipulate and process strings, whether it's for data validation, parsing, or formatting.

## How to:

To extract a substring in Rust, we use the `get` method of the `String` type. Let's say we have the string `Hello, World!` and we only want to extract the word `World`. We can use the following code:

```Rust
let my_string = String::from("Hello, World!");
let my_substring = my_string.get(7..12).unwrap();
println!("{}", my_substring); // outputs "World"
```

Here, we specify the range of indexes (7 to 12) to extract from the `my_string` variable. The `get` method returns an `Option<&str>` so we use `unwrap` to get the actual string value.

We can also use the `find` method to extract a substring based on a pattern. For example, if we want to extract the word `World` from the same string above, we can use this code:

```Rust
let my_pattern = "rld";
let my_substring = my_string.find(my_pattern).unwrap();
println!("{}", my_substring); // outputs "rld"
```

The `find` method returns the starting index of the pattern in the string, which we can use to extract the substring.

## Deep Dive:

In Rust, strings are stored as a sequence of UTF-8 bytes, making substring extraction a bit more complex compared to other languages. This is because indexing strings based on the number of bytes may result in invalid or incomplete characters. To handle this, Rust has the concept of string slices, which are references to a portion of a string.

There are also alternative ways to extract substrings in Rust, such as using regular expressions or the `split` method. Regular expressions allow for more complex pattern matching, while the `split` method allows for splitting a string into smaller strings based on a delimiter.

## See Also:

- [The Rust Standard Library](https://doc.rust-lang.org/std/string/struct.String.html)
- [Regular Expressions in Rust](https://doc.rust-lang.org/std/str/struct.Regex.html)
- [Splitting Strings in Rust](https://doc.rust-lang.org/std/primitive.str.html#method.split)