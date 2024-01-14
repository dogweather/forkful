---
title:    "Rust recipe: Deleting characters matching a pattern"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why
Deleting characters matching a pattern can be useful when cleaning up data or removing unnecessary elements from a string. It can also be used to implement certain algorithms or search functions. In Rust, this can be done efficiently and easily with the built-in methods and libraries.

## How To
To delete characters matching a pattern in Rust, we can use the `replace` method from the `String` type. This method takes in two parameters: the pattern to be replaced and the new string to replace it with.

```
Rust
let original_string = "Hello, world!";
let new_string = original_string.replace("o", ""); // "Hell, wrld!"
```

However, this only replaces the first occurrence of the pattern. To delete all occurrences, we can use the `replace_all` method from the same `String` type.

```
Rust
let original_string = "banana";
let new_string = original_string.replace_all("a", ""); // "bnn"
```

As we can see from the output, the `replace_all` method successfully deleted all "a" characters from the string.

We can also use regular expressions to delete characters matching a pattern. Rust has a built-in `Regex` library that allows us to easily use regular expressions.

```
Rust
use regex::Regex;

let original_string = "1 2 3 hello";
let regex = Regex::new("[0-9]").unwrap(); // pattern to match all numbers
let new_string = regex.replace_all(original_string, ""); // "hello"
```

In this example, we used a regular expression to match all numbers in the string and deleted them, leaving only the word "hello" in the new string.

## Deep Dive
When using the `replace` and `replace_all` methods, it is important to keep in mind that they return a new string rather than modifying the existing one. This is because strings in Rust are immutable, meaning they cannot be changed once created.

Additionally, the `replace` and `replace_all` methods are case-sensitive. This means that if we want to delete both uppercase and lowercase versions of a character, we will need to use two separate methods or regular expressions.

Another important thing to note is that these methods will replace the entire match, not just a single character. For example, if we use the regular expression `"[0-9]+"` to match numbers in a string, it will replace the entire number instead of just a single digit. This can lead to unexpected results if we are not careful.

## See Also
- [The Rust Programming Language Book](https://doc.rust-lang.org/book/)
- [Rust Standard Library Documentation](https://doc.rust-lang.org/std/)
- [Regex Crate Documentation](https://docs.rs/regex/)

Deleting characters matching a pattern in Rust can be a useful tool in your coding arsenal. Whether you are cleaning up data or implementing algorithms, understanding how to use the `replace` and `replace_all` methods, as well as regular expressions, can greatly improve your code.