---
title:                "एक टेक्स्ट फाइल पढ़ना"
html_title:           "Rust: एक टेक्स्ट फाइल पढ़ना"
simple_title:         "एक टेक्स्ट फाइल पढ़ना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Text files are a common type of file that contain human-readable text using characters, such as letters, numbers, symbols, and spaces. Reading a text file means accessing and retrieving the contents of the file. Programmers often need to read text files because they contain important information that can be used in their code.

## How to:

To read a text file in Rust, we can use the `std::fs` module, specifically the `fs::read_to_string()` function. This function takes in the path to the text file as an argument and returns a `Result` enum. Below is an example of how we can use this function to read a text file and print its contents:

```Rust
use std::fs;

// Read the text file
let result = fs::read_to_string("./text_file.txt");

// Check if reading was successful
match result {
    Ok(contents) => println!("{}", contents),
    Err(e) => println!("Error reading file: {}", e),
}
```

The above code will read the text file named `text_file.txt` located in the same directory as our Rust code. The `read_to_string()` function returns an `Ok` variant with the contents of the file if reading was successful, or an `Err` variant with an error message if reading failed.

## Deep Dive:

Reading text files has been an essential part of programming for decades, as text files have been widely used for storing and exchanging information. Before the popularity of the internet, text files were the main format for storing and sharing data. In Rust, there are other ways to read text files, such as using the `fs::read()` function, which returns a `Vec<u8>` of bytes instead of a string.

The `read_to_string()` function uses the encoding of the current environment, but we can also specify the encoding by using the `read_to_string_with_charset()` function, which takes in a `Charset` enum as an argument. This is useful when working with text files that use different character encodings.

## See Also:

To learn more about reading text files in Rust, check out the official documentation for the `fs` module: https://doc.rust-lang.org/std/fs/index.html. You can also explore other methods for reading text files, such as using the `BufRead` trait from the `std::io` module. Happy coding!