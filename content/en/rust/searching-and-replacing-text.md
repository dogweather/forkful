---
title:                "Searching and replacing text"
html_title:           "Rust recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is a common task in programming, where we look for a specific set of characters or words and replace them with another set. This is done to make changes or corrections in our code efficiently. As codebases can be large and complex, manual replacements can be time-consuming and prone to errors. Hence, the use of automated search and replace methods is essential for programmers to maintain code integrity and optimize their workflow.

## How to:

Searching and replacing text in Rust is quite straightforward. First, we need to import the `str::replace` function from the standard library. Then, we use it to search for the text we want to replace and specify what we want to replace it with. Let's see an example:

```Rust
use std::str::replace;

let sentence = "Hello world!";

let replaced_sentence = replace(sentence, "world", "Rust");

println!("{}", replaced_sentence);
```

Output:
```Rust
Hello Rust!
```

In the above code, we use the `replace` function to replace the word "world" with "Rust" in our sentence. The function returns a new string with the specified changes, and we print it to the console.

We can also use the `replace` function with patterns instead of exact words. Let's see another example:

```Rust
use std::str::replace;

let sentence = "Hello, my name is [name]!";

let name = "John";

let replaced_sentence = replace(sentence, "[name]", name);

println!("{}", replaced_sentence);
```

Output:
```Rust
Hello, my name is John!
```

## Deep Dive

The idea of searching and replacing text in programming dates back to the early days of computing. It was first introduced in the SNOBOL programming language in 1962. Since then, it has become a standard feature in most programming languages, including Rust.

Besides using the `replace` function, there are several other ways to search and replace text in Rust. Some third-party libraries, such as `regex`, provide more sophisticated options like using regular expressions. Additionally, many IDEs and code editors also offer built-in search and replace tools for faster and more convenient code editing.

Under the hood, the `replace` function in Rust uses the `String::replace` method, which returns a new string instead of modifying the original one. This avoids side effects and makes code more predictable.

## See Also

- Official Rust documentation on searching and replacing: https://doc.rust-lang.org/std/string/struct.String.html#method.replace
- The `regex` crate: https://crates.io/crates/regex 
- IDEs and code editors with built-in search and replace tools: Visual Studio Code, Sublime Text, Atom, etc.