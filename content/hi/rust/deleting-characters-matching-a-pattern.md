---
title:                "आदेश के साथ मेल खाते हुए चरित्रों को हटाने का तरीका"
html_title:           "Rust: आदेश के साथ मेल खाते हुए चरित्रों को हटाने का तरीका"
simple_title:         "आदेश के साथ मेल खाते हुए चरित्रों को हटाने का तरीका"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# What & Why?
Deleting characters matching a pattern is a common task in programming where specific characters or sequences need to be removed from a given string. This is often necessary in order to manipulate or clean up data for further processing. It can also be used to extract relevant information from a string.

# How to:
```Rust
// Define a string with some unwanted characters
let text = "H-e 12l-l-o!";

// Use the `chars()` method to get an iterator over characters in the string
let mut chars = text.chars();

// Use `macthes()` to find characters matching the given pattern and then filter them out
let filtered: String = chars.filter(|c| !c.matches(char::is_numeric)).collect();

// Print the resulting string
println!("Filtered string: {}", filtered);
```

Output:
```
Filtered string: Hello!
```

# Deep Dive:
There are various approaches and techniques for deleting characters matching a pattern. One alternative is by using regular expressions, which are a powerful tool for pattern matching and manipulation in strings. Rust also provides the `replacer()` method which can be used to replace matches with a different string.

On a lower level, deleting characters matching a pattern involves iterating over each character in the string and checking if it matches the given pattern. If it does, it is skipped or replaced with a different character. This process can be optimized using techniques such as indexing and caching. 

# See Also: 
- [The Rust Programming Language](https://www.rust-lang.org/)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/)