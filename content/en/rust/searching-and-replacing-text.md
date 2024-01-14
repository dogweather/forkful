---
title:                "Rust recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text is a common task in programming, whether it's correcting typos or making mass changes to the code. In Rust, the process of searching and replacing text is made easier through its powerful string manipulation capabilities. 

## How To

To search and replace text in Rust, we can use the `replace()` function from the `str` type. This function takes two arguments: the substring to be replaced and the new replacement substring. Let's see an example:

```Rust
let mut message = "Hello, world!".to_string(); //convert to String type
message = message.replace("world", "Rust"); //replace "world" with "Rust"
println!("{}", message); //prints "Hello, Rust!"
```

We can also use the `replace_range()` function to replace a specific range of characters in a string. This function takes three arguments: the starting index, the ending index, and the new replacement substring. Here's an example:

```Rust
let mut string = String::from("This is a string!");
string.replace_range(8..16, "replaced"); //replace from index 8 to 16 with "replaced"
println!("{}", string); //prints "This is a replaced!"
```

If we only want to replace the first occurrence of a substring, we can use the `replace_first()` function instead. This function takes the same arguments as `replace()`, but only replaces the first instance of the substring. Let's see an example:

```Rust
let message = "Rust is the best programming language";
println!("{}", message.replace_first("the best", "awesome")); //prints "Rust is awesome programming language"
```

## Deep Dive

When using the `replace()` function, it's important to note that it returns a new string rather than modifying the original one. This is because Rust strings are immutable, meaning they cannot be changed after creation. So if we want to modify the original string, we need to use the assignment operator as shown in the examples above. 

Another thing to keep in mind is that the `replace()` function only replaces the first occurrence of a substring by default. If we want to replace all instances, we need to use the `replace_all()` function. This function takes the same arguments as `replace()` and replaces all instances of the substring. 

Lastly, it's worth mentioning that the `replace()` and `replace_range()` functions return a `Result` type, which can handle errors if the replacements cannot be made. This is helpful in ensuring safe coding practices and avoiding potential bugs. 

## See Also

- [Rust Documentation on strings](https://doc.rust-lang.org/std/string/)
- [Replacing all occurrences in a string](https://doc.rust-lang.org/std/string/struct.String.html#method.replace_all)
- [The power of Rust's pattern matching](https://doc.rust-lang.org/book/ch18-02-refutable-and-irrefutable-patterns.html)