---
title:                "Concatenating strings"
html_title:           "Gleam recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings is the process of combining two or more strings together into one string. This is a common task in programming, as it allows for the creation of more complex and dynamic strings. 
 
## How to:

```
Gleam program 

import gleam/string 

fn main() { 
  let first = "Hello "; 
  let second = "World!"; 
  let combined = string.concat(first, second); 
  
  // Output: Hello World! 
  io.println("combined"); 
} 
```
 
In the above code, we import the `gleam/string` module which provides the `concat` function. This function takes in two string arguments and returns a new string that is the combination of those two strings. We then print out the result using the `io.println` function. 
 
## Deep Dive 
 
The process of concatenating strings has been used in programming for a long time. It is often used for tasks such as creating user-friendly messages, formatting data, and building dynamic web pages. 
 
There are other methods for combining strings, such as interpolation and formatting, but concatenation is a simple and straightforward method. 
 
Internally, the Gleam `concat` function uses the Rust `String` type which allows for efficient and flexible manipulation of strings. 
 
## See Also 
 
- [Gleam documentation](https://gleam.run/documentation/)
- [Rust documentation on strings](https://doc.rust-lang.org/std/string/struct.String.html)