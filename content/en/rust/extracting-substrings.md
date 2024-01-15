---
title:                "Extracting substrings"
html_title:           "Rust recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why
String manipulation is a fundamental part of any programming language, and Rust is no exception. With the ability to easily extract and manipulate substrings, you can efficiently work with strings and improve the overall functionality of your code.

## How To
Extracting substrings in Rust is a simple process that can be done using the ```get``` method. First, we will declare a string variable and assign it a value:
```Rust
let sentence = "I love Rust programming";
```
Next, we will use the ```get``` method to extract a substring from this string. The syntax for using ```get``` is as follows:
```Rust
let substring = sentence.get(start_index..end_index); 
```
This will extract the characters between the start and end index and store them in the ```substring``` variable. Let's see this in action:
```Rust
let substring = sentence.get(7..11); //This will extract 'Rust'
println!("{}", substring); //Output: Rust
```
We can also use the ```get``` method with variables instead of using definite indexes. For example:
```Rust
let start_index = 2;
let end_index = 6;
let substring = sentence.get(start_index..end_index); //This will extract 'love'
println!("{}", substring); //Output: love
```
We can also use negative indexes to start from the end of the string. For example:
```Rust
let substring = sentence.get(..4); //This will extract 'I lo'
println!("{}", substring); //Output: I lo
```
To extract the substring from the beginning of the string, we can use the ```..end_index``` syntax. For example:
```Rust
let substring = sentence.get(..3); //This will extract 'I'
println!("{}", substring); //Output: I
```
You can also use the ```get``` method with string slices to extract multiple substrings. For example:
```Rust
let substrings = sentence.get(2..6); //This will extract 'rice'
println!("{:?}", substrings); //Output: rice
```
To extract a substring as a string instead of a string slice, we can use the ```to_string()``` method. For example:
```Rust
let substring = sentence.get(12..).to_string(); //This will extract 'Rust programming'
println!("{}", substring); //Output: Rust programming
```

## Deep Dive
Under the hood, the ```get``` method uses the ```Index``` trait to handle string indexing. This trait is implemented for various string types and allows us to use indexing syntax on strings. The ```get``` method takes an index range ```(start_index..end_index)``` as an argument and uses it to access the characters between those indexes. It returns a string slice, which is a borrowed reference to the original string. This makes it an efficient and safe way to extract substrings without having to create new strings.

## See Also
- [Rust Docs for the String Type](https://doc.rust-lang.org/std/string/struct.String.html)
- [Official Rust Book on String Manipulation](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust Playground for practicing substring extraction](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=401b1e94e13cbf3e4e71f4acad587aae)