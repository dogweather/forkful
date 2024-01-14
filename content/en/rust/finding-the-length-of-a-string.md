---
title:                "Rust recipe: Finding the length of a string"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Rust is a powerful and efficient programming language that has gained popularity among developers in recent years. One of its key features is its strong type system, which ensures that variables are properly initialized and accessed. In this post, we will focus specifically on one common task in Rust programming - finding the length of a string. Understanding how to do this is essential for any developer looking to work with strings in Rust.

## How To

In Rust, strings are represented by the `String` type, which is provided by the standard library. To find the length of a string, we can use the `.len()` method on a string object. Let's take a look at an example:

```Rust
fn main() {
    let my_string = String::from("Hello World!");
    println!("The length of the string is {}", my_string.len());
}
```

When we run this code, the output will be:

```
The length of the string is 12
```

As you can see, we first create a new string object called `my_string` and assign it the value "Hello World!". Then, using the `.len()` method, we print out the length of the string to the console.

It's important to note that the `.len()` method returns the number of bytes in the string, not the number of characters. This is because Rust strings are UTF-8 encoded, so some characters may take up more than one byte.

## Deep Dive

Now that we know how to find the length of a string in Rust, let's take a closer look at what is happening behind the scenes. When we call the `.len()` method, Rust actually calculates the size of the string by iterating through each byte and counting them. This is why it is important to remember that the length is in terms of bytes, not characters.

It's also worth mentioning that Rust has a second method for finding the length of a string called `.chars()`. This method returns the number of Unicode characters in the string, rather than the number of bytes. Depending on your program's requirements, you may need to use one or the other.

## See Also

- [Rust Documentation on String](https://doc.rust-lang.org/std/string/struct.String.html)
- [Official Rust Website](https://www.rust-lang.org/)
- [Rust Reddit Community](https://www.reddit.com/r/rust/)