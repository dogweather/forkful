---
title:    "Rust recipe: Concatenating strings"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

String concatenation is a common operation in programming, where two or more strings are combined together to form a new string. This can be useful in a variety of scenarios, such as creating dynamic messages or data structures.

## How To

In Rust, string concatenation can be done using the `+` operator or the `format!` macro. Let's take a look at some examples using both methods.

Using the `+` operator:

```Rust
let greeting = "Hello";
let name = "John";
let message = greeting + " " + name;
println!("{}", message); // Output: Hello John
```

Using the `format!` macro:

```Rust
let country = "Canada";
let capital = "Ottawa";
let info = format!("The capital of {} is {}", country, capital);
println!("{}", info); // Output: The capital of Canada is Ottawa
```

It's important to note that the `+` operator will take ownership of the strings involved, so they cannot be used again after concatenation. However, the `format!` macro does not take ownership, so the original strings can still be used.

In addition, the `format!` macro allows for more complex formatting of strings, such as inserting variables or performing calculations within the string.

## Deep Dive

Behind the scenes, the `+` operator is calling the `push_str` method on the first string and passing the second string as an argument. This means that the first string is being modified and the resulting string is a combination of the two.

The `format!` macro, on the other hand, creates a new `String` object for the result, leaving the original strings untouched. This can be more efficient for larger strings as it avoids unnecessary memory allocations and copying.

Additionally, the Rust compiler performs string concatenation at compile time, meaning it is optimized for performance. This is in contrast to other languages where concatenation is done at runtime.

## See Also

To learn more about string concatenation in Rust, check out the official [Rust documentation](https://doc.rust-lang.org/std/string/struct.String.html#concatenation). You can also explore other string manipulation methods such as appending, replacing, and splitting.

Happy coding! 🚀