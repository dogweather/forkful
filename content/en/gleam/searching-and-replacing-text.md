---
title:                "Searching and replacing text"
html_title:           "Gleam recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is a common practice among programmers, where they look for a specific word or phrase in a codebase and replace it with another. This can be done manually, but it can be time-consuming and prone to errors, especially in larger codebases. Therefore, programmers use tools like Gleam to automate this process and increase efficiency.

## How to:

In Gleam, searching and replacing text is made easy with the `replace_all` function. Let's say we have a string variable `greeting` with the value "Hello, world!". If we want to replace "world" with "Gleam", we can do so with the following code:

```Gleam
let new_greeting = replace_all(greeting, "world", "Gleam")
```

The output of `new_greeting` would be "Hello, Gleam!", with the previous word "world" replaced by "Gleam". It's that simple!

## Deep Dive:

Searching and replacing text has been a crucial element of software development since the early days of programming. Initially, this process was done manually, but as programming languages and codebases grew more complex, automated methods were developed.

Alternatives to the `replace_all` function in Gleam include using regular expressions or other string manipulation methods. However, these can be more complicated and error-prone for certain cases. With Gleam, developers have a simple and reliable function specifically designed for this task.

Internally, the `replace_all` function in Gleam uses the `str::replace` function from the `std.string` module, which is built upon the Rust standard library's `replace` function. This makes it efficient and powerful for handling text replacement in code.

## See Also:

- [Gleam's official website](https://gleam.run/)
- [Gleam's documentation](https://gleam.run/documentation)
- [Rust standard library documentation on `replace`](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)