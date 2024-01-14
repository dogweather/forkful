---
title:                "Rust recipe: Deleting characters matching a pattern"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself needing to delete certain characters from a string in your Rust code? Maybe you want to remove all whitespace or specific punctuation marks. Whatever the reason may be, knowing how to delete characters matching a pattern can come in handy when working with strings in Rust.

## How To

To delete characters matching a pattern in Rust, we can use the `trim_matches` method from the `str` type. This method takes in a character or string pattern and removes any occurrences of it from both the beginning and end of the string.

Here is an example of how we can use `trim_matches` to remove all whitespace from a string:

```Rust
let string = "   Hello Rust  ";
let trimmed = string.trim_matches(' ');
println!("Trimmed string: {}", trimmed);

// Output: Trimmed string: HelloRust
```

As you can see, the `trim_matches` method removed all the whitespace from the original string, leaving us with only the desired characters.

We can also specify a string pattern instead of a single character. Let's say we want to remove all occurrences of "Rust" from a string. We can do so by passing in the string pattern "Rust" to the `trim_matches` method.

```Rust
let string = "Hello Rust, welcome to my code";
let trimmed = string.trim_matches("Rust");
println!("Trimmed string: {}", trimmed);

// Output: Trimmed string: Hello , welcome to my code
```

## Deep Dive

Under the hood, the `trim_matches` method uses pattern matching to identify and remove characters from the string. This makes it a powerful and versatile tool for deleting characters in Rust.

It's also worth noting that the `trim_matches` method returns a string slice, meaning it doesn't modify the original string. If you want to modify the original string, you can use the `to_string` method to convert the string slice back into a `String` type.

## See Also

- [Rust Documentation on Strings](https://doc.rust-lang.org/std/primitive.str.html)
- [Rust Playground](https://play.rust-lang.org/) (try out the code snippets in this post)
- [Rust Book](https://doc.rust-lang.org/book/) (a comprehensive guide to Rust language)