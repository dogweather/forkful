---
title:    "Rust recipe: Reading command line arguments"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Command line arguments are an essential part of any command line program. They allow users to provide input to the program without the need for a graphical user interface. In this blog post, we will explore how to read command line arguments in Rust.

## How To

Reading command line arguments in Rust is quite straightforward. We can use the `std::env` module to access the list of arguments passed to our program.

Let's say we have a program called `my_program` and we want to read two arguments, `name` and `age`. Here's how we can do it in Rust:

````Rust
use std::env;

fn main() {
    // access the arguments passed to our program
    let args: Vec<String> = env::args().collect();
    
    // check if the arguments are present
    if args.len() < 3 {
        // if not, print an error message and exit
        println!("Please provide a name and an age as arguments.");
        return;
    }
    
    // assign the values to variables
    let name = &args[1];
    let age = &args[2];
    
    // print out the name and age
    println!("Hello, {}! You are {} years old.", name, age);
}
````

If we run `my_program` with the arguments `John 30`, the output will be:

```
Hello, John! You are 30 years old.
```

## Deep Dive

So, how does the `env::args()` function work? It returns an iterator of type `Args`, which we then collect into a `Vec<String>`. This vector contains the name of our program as the first element and all the arguments passed to the program as subsequent elements.

We then check the length of the vector to ensure that the correct number of arguments were provided. If not, we print an error message and exit the program.

Next, we use indexing to assign the value of the arguments to variables. It's important to note that the first element of the vector is the name of our program, so we need to start our indexing at 1.

## See Also

- [Rust Documentation for std::env::args](https://doc.rust-lang.org/stable/std/env/fn.args.html)
- [The Rust Book - Command Line Programs](https://doc.rust-lang.org/book/ch12-00-an-io-project.html)

Now that you know how to read command line arguments in Rust, you can start creating powerful command line programs!