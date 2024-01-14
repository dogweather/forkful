---
title:    "Rust recipe: Capitalizing a string"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why

Capitalizing a string is a task that is commonly used in programming to make text appear in a certain format. Whether it's for aesthetic purposes or for data processing, it is a useful skill to have in your programming arsenal.

## How To

To capitalize a string in Rust, we will be using the `to_uppercase()` method for strings. Let's take a look at a simple example:

```Rust
let my_string = String::from("hello, world!");

let capitalized_string = my_string.to_uppercase();

println!("Capitalized string: {}", capitalized_string);
```

The output of this code will be:

```
Capitalized string: HELLO, WORLD!
```

As you can see, the `to_uppercase()` method automatically converts each character in the string to its uppercase equivalent. This method can be applied to any String variable, making it a simple and effective way to capitalize a string.

But what if you only want to capitalize the first letter of the string? In that case, we can use a combination of methods `chars()` and `enumerate()` to iterate through the string and capitalize the first character. Here's an example:

```Rust
let my_string = String::from("hello, world!");

let capitalized_first = my_string
    .chars()
    .enumerate()
    .map(|(i, c)| if i == 0 { c.to_uppercase() } else { c })
    .collect::<String>();

println!("Capitalized first letter: {}", capitalized_first);
```

The output of this code will be:

```
Capitalized first letter: Hello, world!
```

## Deep Dive

Now, let's take a deeper look at the `to_uppercase()` method and its implementation in Rust. The method is part of the `ToString` trait, which is implemented for all types that can be converted into a string. The `to_uppercase()` method uses the current locale to convert the characters, making it language-specific.

It's also important to note that the `to_uppercase()` method doesn't just work for English characters but can also handle characters from various languages such as Japanese, Arabic, and more.

## See Also

- [String - Rust Standard Library Documentation](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust By Example: Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Official Rust Language Website](https://www.rust-lang.org/)