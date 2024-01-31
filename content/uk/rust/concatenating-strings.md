---
title:                "Об'єднання рядків"
date:                  2024-01-20T17:35:49.684161-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"

category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
String concatenation is sticking one string to another. We do this to build up messages, create paths, combine user-generated content, or to simply put pieces of text together.

## How to (Як це робити)
Here's how you glue strings together in Rust:

```Rust
fn main() {
    let hello = "Вітаю".to_string();
    let world = "Світ";
    let exclamation = "!";
    
    // Using the + operator
    let greeting = hello + " " + world + exclamation;
    println!("{}", greeting);  // Вітаю Світ!
    
    // Using format! macro
    let formatted_greeting = format!("{} {}{}", "Вітаю", "Світ", "!");
    println!("{}", formatted_greeting);  // Вітаю Світ!
}
```
Common ways to combine strings in Rust include using the `+` operator or the `format!` macro. Note the difference in borrow-checker requirements: with `+`, `hello` needs to be a `String` (not a `&str`) because the operator takes ownership of `hello`.

## Deep Dive (Поглиблений огляд)
Concatenating strings is as old as programming itself; it's been in use since the early days to allow programs to communicate complex messages. In Rust, string concatenation respects ownership and borrowing rules, which is why `String` and `&str` (string slices) have different behaviors. Alternatives to concatenation also exist, like joining strings in a vector with `join()` method or building up a string with a `StringWriter`.

```Rust
// Using join() method on a vector of strings
let words = vec!["Вітаю", "Світ", "!"];
let joined = words.join(" ");
println!("{}", joined); // Вітаю Світ!

// Using push_str to build up a string
let mut message = String::from("Вітаю");
message.push_str(" Світ");
message.push_str("!");
println!("{}", message); // Вітаю Світ!
```

## See Also (Дивіться також)
To dig deeper into Rust strings:

- The Rust Book on strings: https://doc.rust-lang.org/book/ch08-02-strings.html
- Rust String API Docs: https://doc.rust-lang.org/std/string/struct.String.html
- Rust By Example for strings: https://doc.rust-lang.org/rust-by-example/std/str.html

This should get you started. Щасливого кодування! (Happy coding!)
