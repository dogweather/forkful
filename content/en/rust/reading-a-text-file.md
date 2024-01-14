---
title:    "Rust recipe: Reading a text file"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Are you looking to learn a new programming language that combines the power and performance of languages like C and C++ with the safety and ease of use of modern languages? If so, then Rust is the perfect choice for you! In this blog post, we'll be discussing how to read a text file in Rust.

## How To

Reading a text file in Rust is quite simple. We'll first need to import the necessary libraries using the `use` keyword. Since we'll be dealing with files, we'll need to use the `std::fs` library.

```Rust
use std::fs::File;
use std::io::prelude::*;
```

Next, we'll create a mutable variable to store our file in using the `File::open()` function. This function takes in the name of the file as a parameter and returns a `Result` enum with either a file handle or an error.

```Rust
let mut file = File::open("example.txt").expect("Failed to open file.");
```

Now, we can use the `read_to_string()` method on our file variable to read the contents of the file into a string.

```Rust
let mut file_contents = String::new();
file.read_to_string(&mut file_contents).expect("Failed to read file.");
```

We can then print out the contents of the file to verify that it was read successfully.

```Rust
println!("File contents:\n{}", file_contents);
```

Running this code will output the contents of the text file, giving us a glimpse into the world of text file reading in Rust.

## Deep Dive

Now that we've seen how to read a text file in Rust, let's take a deeper look at what's actually happening behind the scenes. First, we use the `File::open()` function to attempt to open the file. This function returns a `Result` enum, which helps us handle any potential errors that may occur while opening the file.

Next, we use the `read_to_string()` method to read the contents of the file into a mutable string. This method takes in a mutable reference to our `file_contents` variable and reads the contents of the file into it. We use a mutable reference because the `read_to_string()` method needs to modify the string object in order to add the file contents to it.

## See Also

Now that you know how to read a text file in Rust, you can continue to explore the Rust documentation and learn more about the language. Check out the following links for more information and tutorials:

- [The Rust Programming Language](https://doc.rust-lang.org/book/index.html)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/index.html)
- [Rust Crash Course](https://learnxinyminutes.com/docs/rust/)
- [Official Rust Website](https://www.rust-lang.org/)

Happy coding in Rust!