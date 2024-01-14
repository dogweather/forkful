---
title:    "Rust recipe: Converting a string to lower case"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Rust is a powerful and modern programming language that has gained a lot of popularity in recent years. One of its key features is its strong focus on safety and performance. In this blog post, we will explore how to convert a string to lower case in Rust.

## How To

Converting a string to lower case may seem like a simple task, but it can be tricky in languages like Rust where types are enforced. Fear not, as Rust provides a built-in method to easily convert a string to lower case.

Let's first start by declaring a string variable and assigning it a value:

```Rust
let my_string = "Hello World";
```

To convert this string to lower case, we can use the `.to_lowercase()` method provided by Rust's `String` type. This method will return a new string with all characters converted to lower case.

```Rust
let lower_case = my_string.to_lowercase();

println!("Original string: {}", my_string); // Output: Hello World
println!("Lower case string: {}", lower_case); // Output: hello world
```

And that's it! We have successfully converted a string to lower case in Rust. It's important to note that the original string is not modified, but rather a new string is created with lower case characters.

## Deep Dive

Behind the scenes, the `.to_lowercase()` method uses the Unicode standard to perform the conversion. This means that any characters with special casing (such as accented characters) will be properly converted to their lower case equivalent.

It's also worth mentioning that this method is not limited to just ASCII characters, as it supports the conversion of any valid Unicode character.

There are also a few other methods in Rust that can be used for case conversion, such as `.to_uppercase()` which converts a string to upper case, and `.to_ascii_lowercase()` and `.to_ascii_uppercase()` which only work with ASCII characters.

## See Also

For more information on case conversion in Rust, check out the official documentation [here](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase).

You can also explore other useful string methods in Rust [here](https://doc.rust-lang.org/std/string/struct.String.html#methods).