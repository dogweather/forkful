---
title:                "Rust recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Writing a text file is a fundamental task in programming that allows us to store and retrieve data from a file. Whether it's for logging purposes or to save user input, knowing how to write a text file in Rust can greatly improve your coding skills and make your programs more efficient.

## How To
To write a text file in Rust, we first need to open a file in write mode using the `File::create` method. This method takes in the file name as a parameter and returns a `Result` type. We can then use the `expect` method to handle any errors that may occur.

```
let file = File::create("myfile.txt").expect("Could not create file");
```

Next, we need to write data to the file using the `write_all` method. This method takes in a slice of bytes as a parameter, which can be created using the `as_bytes` method. We can also use the `write` method to write data one byte at a time.

```
let data = "This is a sample text";
file.write_all(data.as_bytes()).expect("Could not write to file");
```

After writing the data, we need to close the file using the `close` method to save any changes made to the file.

```
file.close().expect("Could not close file");
```

Now, when we open the file, we can see that our data has been successfully written to it.

```
# Output:
This is a sample text
```

## Deep Dive
When writing a text file in Rust, it is important to handle potential errors that may occur. Rust has a `Result` type which is used to handle errors in a safe and concise manner. The `expect` method we used earlier is just one way to handle errors. Alternatively, we can use the `match` control flow construct to handle different `Result` variants.

```
match file.write_all(data.as_bytes()) {
    Err(e) => panic!("Could not write to file: {}", e),
    Ok(_) => println!("Data successfully written to file"),
}
```

Additionally, Rust provides various options for opening files in different modes such as read, append, and write mode. We can also specify the encoding type we want to use when writing to a text file.

```
let mut file = OpenOptions::new()
    .write(true)
    .append(true)
    .open("myfile.txt").expect("Could not open file");
```

## See Also
- [The Rust Book - File I/O](https://doc.rust-lang.org/book/ch12-00-an-io-project.html)
- [Rust Standard Library - File](https://doc.rust-lang.org/std/io/struct.File.html)
- [Rust By Example - File I/O](https://doc.rust-lang.org/stable/rust-by-example/std_misc/file.html)