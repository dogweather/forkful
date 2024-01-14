---
title:    "Rust recipe: Finding the length of a string"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Why

Have you ever struggled with finding the length of a string while coding? Maybe you need to restrict the input of a user to a maximum number of characters, or you simply want to display the number of characters in a string on your UI. Regardless of the reason, knowing how to find the length of a string in Rust can be a useful skill to have.

# How To

To find the length of a string in Rust, we can use the `.len()` method. This method is available for all types that implement the `Sized` trait, including strings. Let's take a look at a simple example:

```Rust
let my_string = "Hello, Rust!";
println!("The length of my string is {}", my_string.len());
```

The output of this code will be:

```
The length of my string is 13
```

In this example, we first create a string called `my_string`. Then, we use the `.len()` method to return the length of the string and print it out using `println!()`.

We can also use this method on user input, for example, if we want to restrict the input to a maximum of 10 characters. Here's how we can do that:

```Rust
use std::io;

let mut input = String::new();

// Get input from user
match io::stdin().read_line(&mut input) {
    Ok(_) => {
        // Trim the input to remove any whitespaces
        input = input.trim().to_string();
        if input.len() > 10 {
            // If length is greater than 10, display an error
            println!("Input exceeded the maximum length of 10 characters!");
        } else {
            // Otherwise, display the input
            println!("Your input: {}", input);
        }
    }
    Err(_) => println!("Error reading input"),
}
```

Here, we first create a mutable string using `String::new()`. Then, we use `io::stdin()` to get user input and check its length using `input.len()`. If the length is greater than 10, we display an error, and if it's less than or equal to 10, we display the input.

# Deep Dive

It's important to note that the `.len()` method returns the number of bytes in a string, not the number of characters. This means that for strings containing non-ASCII characters, the length may not be what we expect. For example, the string "こんにちは" (which means "hello" in Japanese) has a length of 15 bytes, but only contains 5 characters.

If we need to find the number of characters in a string instead of the number of bytes, we can use the `.chars()` method, which returns an iterator over the characters in a string. We can then use the `.count()` method to count the number of characters in the string. Here's an example:

```Rust
let my_string = "こんにちは";
println!("The length of my string is {}", my_string.chars().count());
```

The output of this code will be:

```
The length of my string is 5
```

# See Also

- Rust documentation on `.len()` and `.chars()`: https://doc.rust-lang.org/std/string/struct.String.html#method.len
- Tutorial on Rust strings: https://doc.rust-lang.org/book/ch08-02-strings.html