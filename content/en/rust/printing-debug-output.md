---
title:    "Rust recipe: Printing debug output"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of software development, and it becomes even more crucial when working on complex projects in languages like Rust. One of the most useful tools for debugging is printing debug output, which allows you to get a closer look at what's happening within your code. In this blog post, we'll discuss the importance of printing debug output and how to do it in Rust.

## How To

Printing debug output in Rust is a simple yet powerful way to understand what's happening in your code. To do so, we use the `println!` macro, which prints the specified text to the console. Let's look at an example:

```Rust
fn main() {
    let x = 5;
    println!("The value of x is {}", x);
}
```

In this example, we declare a variable `x` with the value of 5, and then use the `println!` macro to print the value of `x` to the console. The `{}` is a placeholder for the value of `x`, and the result of running this code would be:

```
The value of x is 5
```

You can also print multiple variables and use formatting options within the `println!` macro. For example:

```Rust
fn main() {
    let x = 5;
    let y = 10;
    println!("The sum of {} and {} is {}", x, y, x + y);
}
```

The output for this would be:

```
The sum of 5 and 10 is 15
```

This is just a basic example, but you can use `println!` to print out any variable or expression in your code, making it easier to understand and identify any errors.

## Deep Dive

While `println!` is a straightforward and useful tool for printing debug output, it's not the only option available in Rust. Rust also has a standard library module called `std::debug`, which provides a `Debug` trait that implements the `fmt::Debug` trait. This allows you to print out the debug representation of any data type, making it easier to inspect complex structures. You can use the `dbg!` macro to print out the debug representation of a variable or expression directly to the console.

```Rust
fn main() {
    let language = "Rust";
    dbg!(language);
}
```

The output for this would be:

```
[Rust] language = "Rust"
```

This is especially useful for large data structures, as it provides a more structured and organized output compared to using `println!`.

## See Also

- Rust Programming Language: https://www.rust-lang.org/
- `println!` documentation: https://doc.rust-lang.org/std/macro.println.html
- `dbg!` documentation: https://doc.rust-lang.org/std/macro.dbg.html