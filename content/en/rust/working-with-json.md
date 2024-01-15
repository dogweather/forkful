---
title:                "Working with json"
html_title:           "Rust recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON (JavaScript Object Notation) is a popular data format used for storing and transferring data between applications. As a Rust programmer, understanding how to work with JSON can greatly expand your abilities and make your code more versatile.

## How To

To work with JSON in Rust, you will need to use a crate (Rust's term for libraries) called `serde_json`. This crate provides convenient methods for serializing and deserializing JSON data.

Let's start by creating a simple JSON object using the `serde_json` crate:

```rust
use serde_json::json;

let person = json!({
    "name": "John Doe",
    "age": 28,
    "hobbies": ["hiking", "reading", "coding"]
});
```

We can easily access values within the JSON object using dot notation:

```rust
let name = person["name"].as_str(); // returns Some("John Doe")

let hobbies = person["hobbies"].as_array(); // returns Some(["hiking", "reading", "coding"])
```

We can also modify the JSON object or add new values:

```rust
person["age"] = json!(30); // changes the age value to 30

person["occupation"] = json!("software engineer"); // adds a new key-value pair
```

To convert our JSON object into a string, we can use the `.to_string()` method:

```rust
let json_string = person.to_string(); // returns {"name":"John Doe","age":30,"hobbies":["hiking","reading","coding"],"occupation":"software engineer"}
```

We can also deserialize a JSON string into a Rust data structure using the `from_str` method:

```rust
use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
struct Person {
    name: String,
    age: u8,
    hobbies: Vec<String>,
    occupation: Option<String>,
}

let json_string = r#"{"name":"Jane Doe","age":25,"hobbies":["painting","dancing"],"occupation":"artist"}"#;

let person: Person = serde_json::from_str(json_string).unwrap();

println!("{}", person.name); // prints Jane Doe
```

## Deep Dive

Working with JSON in Rust becomes even more powerful when combined with Rust's type system. The `serde_json` crate allows us to serialize and deserialize more complex data structures, such as enums and structs, by implementing the `Serialize` and `Deserialize` traits respectively.

The crate also supports custom serialization and deserialization logic through the use of attributes, allowing for even more flexibility in handling JSON data.

## See Also

- Official documentation for the `serde_json` crate: https://docs.serde.rs/serde_json/
- Rust Book chapter on working with JSON data: https://doc.rust-lang.org/book/ch08-03-hash-maps.html#working-with-json-data