---
title:                "Rust recipe: Printing debug output"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Printing debug output is an essential part of the debugging process in any programming language. It allows developers to get a deeper understanding of what is happening behind the scenes in their code and to identify and fix any issues that may arise.

## How To

Printing debug output in Rust is quite simple and can be done using the `println!()` macro. Let's take a look at an example:

```Rust
fn main() {
    let name = "John";
    let age = 28;
    println!("My name is {} and I am {} years old.", name, age);
}
```

Running this code will print the following output:

```
My name is John and I am 28 years old.
```

You can also include more complex data types in your debug output by using the `{:?}` format specifier. Let's say we have a struct representing a person:

```Rust
struct Person {
    name: String,
    age: u32,
    profession: String,
}

fn main() {
    let john = Person {
        name: String::from("John"),
        age: 28,
        profession: String::from("Software Engineer"),
    };
    println!("Debug output: {:?}", john);
}
```

The output will be:

```
Debug output: Person { name: "John", age: 28, profession: "Software Engineer" }
```

As you can see, the `{:?}` format specifier prints out the debug representation of the `Person` struct.

## Deep Dive

In addition to the `println!` macro, Rust also provides the `dbg!` macro for more advanced debugging needs. It not only prints out the value of the variable but also the file and line where it was called from. Let's see an example:

```Rust
fn main() {
    let x = 5;
    let y = 10;
    let z = dbg!(x + y);
}
```

The output will be:

```
[src/main.rs:4] x + y = 15
```

This can be very useful when debugging complex code with multiple variables and functions.

## See Also
- [Rust Documentation: std::fmt - Formatting](https://doc.rust-lang.org/stable/std/fmt)
- [Rust By Example: Formatted Printing](https://doc.rust-lang.org/stable/rust-by-example/hello/print/print_debug.html)
- [The Rust Book: Printing to the Console](https://doc.rust-lang.org/book/ch01-02-hello-world.html#printing-to-the-console)