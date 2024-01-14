---
title:                "Rust recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

In programming, deleting characters that match a certain pattern is a common task. Whether it's cleaning up user input or removing unwanted characters from a string, this can be a useful skill to have in your programming arsenal. In this blog post, we'll explore how to efficiently delete characters matching a pattern in the Rust programming language.

## How To

To delete characters that match a pattern in Rust, we can use the `replace` method from the standard library's `String` type. This method takes in a pattern and a replacement string and returns a new string with all occurrences of the pattern replaced with the replacement string.

Let's take a look at an example:

```Rust
let str = "Hello,world!";
let new_str = str.replace(",", "");
```

In this code, we have a string that contains a comma between the words "Hello" and "world". Using the `replace` method, we provide the comma as our pattern and an empty string as our replacement. The output will be a string without the comma, which is equal to "Hello world!".

We can also use the `replace` method to remove multiple characters that match a specific pattern. We can provide a string as our pattern and an empty string as the replacement, and all the characters in the pattern string will be removed.

Let's see another example:

```Rust
let str = "This is a test string!";
let new_str = str.replace("aeiou", "");
```

In this code, we use the `replace` method to remove all vowels from the string. The result will be "Ths s  tst strng!".

## Deep Dive

Under the hood, the `replace` method is using the `std::mem::replace` function, which replaces all occurrences of a pattern in a string slice with a replacement string. It also handles Unicode characters properly, making it a safe and efficient method to use.

Additionally, if we want to only remove characters from a string without replacing them with anything, we can use the `retain` method from the `String` type. This method takes a closure as an argument, and any character that returns `false` from the closure will be removed from the string.

Here's an example:

```Rust
let str = "123abc456";
let new_str = str.retain(|c| c.is_numeric()); 
```

This code will keep only the numbers in the string and remove all the alphabetic characters, resulting in "123456".

## See Also

If you want to dive deeper into string manipulation in Rust, here are some recommended resources:

- [Rust standard library documentation on String](https://doc.rust-lang.org/std/string/index.html)
- [Official Rust book chapter on strings and operations](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html)
- [Rust Playground](https://play.rust-lang.org/) for quick and easy coding experiments.