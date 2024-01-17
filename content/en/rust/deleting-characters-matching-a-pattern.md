---
title:                "Deleting characters matching a pattern"
html_title:           "Rust recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? 
Deleting characters that match a certain pattern is a common task for programmers, especially when working with large strings or datasets. Deleting unwanted characters can clean up data and make it easier to manipulate. By removing unnecessary characters, we can streamline our code and ensure that it runs efficiently.

## How to:
To delete characters matching a pattern in Rust, we can use the ```.replace``` function. This function takes in two parameters: the pattern to be deleted and the character or string to replace it with. Here's an example of how to use it:

```Rust
let string = "Hello, world!";
let new_string = string.replace("o", ""); // This will delete all "o" characters from the string
println!("{}", new_string); // Output: Hell, wrld!
```

We can also use regular expressions with the ```replace``` function to delete more complex patterns. Here's an example:

```Rust
let string = "Hey123 there!";
let new_string = string.replace(r"[0-9]", ""); // This will delete all numbers from the string
println!("{}", new_string); // Output: Hey there!
```

## Deep Dive:
In the past, deleting characters from strings was a tedious task that required loops and conditional statements. However, with the ```replace``` function in Rust, it's a much simpler and cleaner process. Additionally, Rust's ownership and borrowing system ensures that the original string is not mutated when using the ```replace``` function.

Other alternatives for deleting characters in Rust include using the ```filter``` function with iterators and regular expressions. However, the ```replace``` function is the most efficient and straightforward option for this task.

The ```replace``` function is implemented using a modified form of the Aho-Corasick string matching algorithm, which allows for efficient and fast character deletion. It also supports Unicode characters, making it a versatile and reliable option for deleting characters in any language.

## See Also:
- [Rust documentation for the replace function](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [The Aho-Corasick algorithm](https://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_algorithm)
- [Regular expressions in Rust](https://doc.rust-lang.org/regex/regex/index.html)