---
title:                "Rust recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON (JavaScript Object Notation) is a popular data format used for transmitting and storing data. It is widely used in web development and can be found in many APIs, making it a valuable skill for any Rust programmer. Understanding how to work with JSON in Rust can open up opportunities for building robust and scalable applications.

## How To

Working with JSON in Rust is made easy with the serde and serde_json crates. These crates provide functions for serializing and deserializing JSON data. Let's take a look at an example of how to serialize a struct into JSON using serde:

```
Rust
extern crate serde;
extern crate serde_json;

use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
}

fn main() {
    let person = Person {
        name: String::from("John"),
        age: 25,
    };
    let json = serde_json::to_string(&person).unwrap();
    println!("{}", json);
}
```

The output of this code would be: `{"name": "John", "age": 25}`. This shows how easy it is to convert Rust data structures into JSON using serde.

Deserializing JSON data into Rust structs is just as simple. Let's use the same Person struct from before, but this time with some JSON data:

```
Rust
extern crate serde;
extern crate serde_json;

use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
}

fn main() {
    let json = r#"{ "name": "Jane", "age": 30 }"#;
    let person: Person = serde_json::from_str(json).unwrap();
    println!("Name: {}, Age: {}", person.name, person.age);
}
```

This would output: `Name: Jane, Age: 30`, showing how easy it is to convert JSON data into Rust structs.

## Deep Dive

Working with JSON in Rust can get more complex when dealing with nested data structures or handling errors. The serde crate has advanced features such as custom deserializers, allowing for more flexibility in handling different types of JSON data.

Additionally, the serde_json crate has support for pretty printing, making it easier to read and debug your JSON data. This can be done by passing in the `PrettyFormatter` when serializing the data.

## See Also

For more information on working with JSON in Rust, check out these resources:

- Serde documentation: https://serde.rs/
- Serde JSON documentation: https://serde.rs/json.html
- Rust Cookbook: https://rust-lang-nursery.github.io/rust-cookbook/web/clients/json.html