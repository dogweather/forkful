---
title:    "Rust recipe: Printing debug output"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why

When writing code, it is often necessary to debug and troubleshoot any issues that may arise. One useful tool for this is printing debug output. By printing specific values or variables during the execution of our program, we can gain insights into its behavior and identify any potential bugs. In Rust, the `println!` macro is commonly used for this purpose.

## How To

To print debug output in Rust, first import the `println!` macro from the standard library by adding `use std::println;` at the top of your code. Then, use the macro followed by a string literal containing the desired output. Any values or variables can be included within curly braces `{}` to be printed out. 

```Rust
use std::println;

fn main() {
    let name = "John";
    let age = 25;
    println!("My name is {} and I am {} years old.", name, age);
}
```
 This code will output: `My name is John and I am 25 years old.`

We can also use the `format!` macro to store the debug output in a string variable instead of printing it directly. This can be useful for more complex debugging scenarios.

```Rust
use std::format;

fn main() {
    let num1 = 10;
    let num2 = 5;
    let result = format!("{} divided by {} is equal to {}", num1, num2, num1/num2);
    println!("{}", result);
}
```
This code will output: `10 divided by 5 is equal to 2`.

## Deep Dive

Rust also has a `dbg!` macro which can be used for more advanced debugging. It not only prints the value of a variable, but also its data type and line number of the code where it is called. This can be helpful in pinpointing the exact location of any potential issues.

```Rust
fn main() {
    let mut list = vec![1, 2, 3];
    println!("{:?}", list); // prints the entire vector
    println!("{:?}", list.pop()); // prints and removes the last element of the vector
    dbg!(list); // print the vector, its data type, and line number of this statement
}
```
This code will output: `[1, 2, 3]`, `Some(3)`, and `dbg!(list)` prints `vec![1, 2]`.

## See Also

For more information on debugging techniques in Rust, check out these resources:

- [Debugging Rust Programs - The Rust Book](https://doc.rust-lang.org/book/ch12-00-an-io-project.html#testing-the-librarys-functionality)
- [Using println! and format! to Print Values in Rust - Medium](https://medium.com/@saschagrunert/debugging-rust-code-with-println-and-format-42122ec6c192)
- [Rust Debugging Tools - Official Rust Documentation](https://doc.rust-lang.org/std/macro.dbg.html)