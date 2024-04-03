---
date: 2024-01-25 02:59:56.448373-07:00
description: "Organizing code into functions is about breaking your program into reusable,\
  \ modular chunks identified by a name. We do it to make our code cleaner, more\u2026"
lastmod: '2024-03-13T22:44:59.900896-06:00'
model: gpt-4-1106-preview
summary: Organizing code into functions is about breaking your program into reusable,
  modular chunks identified by a name.
title: Organizing code into functions
weight: 18
---

## What & Why?
Organizing code into functions is about breaking your program into reusable, modular chunks identified by a name. We do it to make our code cleaner, more readable, and easier to debug. It's about not repeating ourselves and streamlining updates.

## How to:
Say you've got code that calculates the area of a circle multiple times. Instead of repeating the formula, you wrap it into a function.

```Rust
fn calculate_circle_area(radius: f64) -> f64 {
    std::f64::consts::PI * radius.powi(2)
}

fn main() {
    let radius = 5.0;
    let area = calculate_circle_area(radius);
    println!("The area of the circle is: {}", area);
}
```

Output:

```
The area of the circle is: 78.53981633974483
```

## Deep Dive
Historically, functions come from math, where they map inputs to outputs. In coding, they've been around since the assembly days, though we called them 'subroutines'. Rust functions can return values and even other functions thanks to first-class functions and closures.

Alternatives? Inline code or macros, but they're messy for complex logic. Objects with methods are another way to organize functionality, a different flavor than standalone functions.

Implementation in Rust is pretty straightforward. Functions declare their parameter types and return type. They're 'snake case' for naming by convention. You've got your public functions (`pub fn`) for use outside the module and private ones for internal use. And Rust has this cool feature where you don't need a `return` keyword for the last expression in a function.

## See Also
Check out these for more info:
- The Rust Programming Language Book: [Functions](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- Rust by Example on [Functions](https://doc.rust-lang.org/rust-by-example/fn.html)
