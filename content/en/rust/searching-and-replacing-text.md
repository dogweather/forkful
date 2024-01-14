---
title:    "Rust recipe: Searching and replacing text"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text is a common task in programming, especially when working with large amounts of data or complex code. With Rust, a systems programming language known for its speed, safety, and concurrency, this task can be efficiently and reliably accomplished.

## How To

To search and replace text in Rust, we can use the `replace` method from the standard library's `String` type. Let's start with a simple example where we want to replace all occurrences of "hello" with "hi" in a string:

```Rust
let mut message = "Hello, world!".to_string();
message.replace("hello", "hi");
println!("{}", message);

// Output: Hi, world!
```

We use the `replace` method and provide the text we want to replace as the first argument, and the replacement text as the second argument. The `replace` method returns a new string with the replacements made, so we need to assign it to a variable to see the changes.

We can also use regular expressions for more advanced search and replace operations. Let's see how we can replace all digits in a string with "X":

```Rust
extern crate regex;
use regex::Regex;

let mut message = "I have 7 apples and 3 bananas".to_string();
let re = Regex::new(r"\d").unwrap();
message = re.replace_all(&message, "X").to_string();
println!("{}", message);

// Output: I have X apples and X bananas
```

Here, we first import the `regex` crate and create a `Regex` object. We then use the `replace_all` method, passing in the string and replacement as arguments. Finally, we convert the result to a string and print it out.

## Deep Dive

The `replace` method is a part of the `From<&str>` trait, which allows us to use it on any type that can be converted into a string slice, such as `&String` or `&str`.

The `replace` method uses a `pattern` and `substitution` string to find and replace the text. Both of these strings can be either `&str` or `&String`, meaning we can use either a string literal or a variable containing a string.

Additionally, the `replace` method has a `max` parameter which specifies the maximum number of replacements to make. By default, this is set to `usize::MAX`, meaning all occurrences of the pattern will be replaced. However, we can specify a lower number to only replace a certain number of occurrences.

## See Also

- [Official Rust Documentation on String](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Rust Regex Crate Documentation](https://docs.rs/regex/1.4.3/regex/)

In conclusion, searching and replacing text in Rust is a straightforward task using the `replace` method. By understanding its various parameters and using regular expressions, we can efficiently manipulate strings in our programs. Happy coding in Rust!