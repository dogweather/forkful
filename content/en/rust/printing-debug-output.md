---
title:                "Printing debug output"
html_title:           "Rust recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is the practice of displaying specific information about the code's execution as it runs in order to aid the programmer in identifying bugs and errors. Programmers use this technique to better understand the behavior of their code and troubleshoot any issues that may arise during development.

## How to:

To print debug output in Rust, use the ```println!()``` macro. This macro takes a string as an argument and displays it on the console when the code is executed. Here's an example:

```
fn main() {
  let num1 = 5;
  let num2 = 10;
  println!("The value of num1 is {} and the value of num2 is {}", num1, num2);
}
```
This will print the following output on the console:
```
The value of num1 is 5 and the value of num2 is 10
```
The exclamation mark at the end of ```println!``` indicates that it is a macro rather than a function. This allows it to take a variable number of arguments. You can use this macro to display any variable or expression by adding them as arguments within the curly braces {}

## Deep Dive:

Historically, printing debug output was a common practice in programming languages. It originated in the early days of computing when mainframe computers were the norm and debugging tools were not yet available. Programmers would have to write out their code and manually check its execution by printing out values to see if their code was behaving as expected.

Today, there are many alternatives to printing debug output such as using a debugger or logging libraries. However, this practice is still widely used in Rust due to its simplicity and effectiveness in debugging code.

When using ```println!```, you may also want to use formatting options to show the data in a specific way. For example, you can use the ```{:?}``` formatting option to print out a variable's value in a debug-friendly format. This can be useful when working with complex data structures.

## See Also:

- [Rust Documentation on ```println!```](https://doc.rust-lang.org/std/macro.println.html)
- [Debugging in Rust with ```dbg!```](https://danielkeep.github.io/practical-intro-to-rust/development-setup/debugging.html)
- [Logging in Rust with the ```log``` crate](https://docs.rs/log/0.4.8/log/)