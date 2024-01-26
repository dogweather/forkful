---
title:                "Organizing code into functions"
date:                  2024-01-25T02:59:30.907412-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizing code into functions"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## What & Why?
Organizing code into functions means breaking down a program’s behavior into smaller, reusable chunks. Programmers do this to make code clearer, more maintainable, and to avoid repetition.

## How to:
Here’s a simple example of organizing code into functions in Gleam:

```gleam
fn add(x, y) {
  x + y
}

fn main() {
  let sum = add(3, 4)
  sum
}

// Sample output
// 7
```

In this snippet, `add` is a function that takes two values and adds them. `main` is where we call `add` and manage the result.

## Deep Dive
Historically, the concept of functions (or 'subroutines') revolutionized programming, paving the way for structured programming in the 1960s and beyond. Functions encourage a modular approach, where problems are divided into sub-problems, solved independently, and composed to solve the larger issue.

In Gleam, which is strongly typed, functions also carry type information, ensuring their use is consistent with their definition. This reduces errors and clarifies intentions.

Alternatives to functions include inline coding, where the logic is repeatedly written out. While sometimes faster for small, one-off tasks, inline coding doesn’t scale well for larger applications.

Implementation details to consider when organizing into functions may include function composition, where functions are used as building blocks, and higher-order functions, which take other functions as arguments or return them, adding flexibility to how code is organized and executed.

## See Also
For more on functions in Gleam, you can dive into the official documentation at:
- [Gleam language functions](https://gleam.run/book/tour/functions.html)

Or explore broader programming concepts:
- [Mozilla Developer Network on JavaScript Functions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions)
- [Learn You Some Erlang for Great Good! - On Modules and Functions](https://learnyousomeerlang.com/modules)
