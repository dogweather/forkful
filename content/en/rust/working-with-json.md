---
title:                "Working with json"
html_title:           "Arduino recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) is a text format for data exchange. Programmers use it for its simplicity and language interoperability, making data sharing between services and applications seamless.

## How to:

In Rust, `serde` and `serde_json` crates are the go-to libraries for JSON work. Here's how to use them:

First, add dependencies in `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

Then, import the crates and define a struct to represent your data:

```rust
extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate serde_derive;

#[derive(Serialize, Deserialize, Debug)]
struct User {
    id: u64,
    name: String,
    email: String,
}

fn main() {
    // Serialize
    let user = User {
        id: 1,
        name: "Jane Doe".to_string(),
        email: "jane.doe@example.com".to_string(),
    };
    let j = serde_json::to_string(&user).unwrap();
    println!("{}", j); // {"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}

    // Deserialize
    let e: User = serde_json::from_str(&j).unwrap();
    println!("{:?}", e);  // User { id: 1, name: "Jane Doe", email: "jane.doe@example.com" }
}
```

## Deep Dive:

The `serde` library ecosystem is Rust's de facto solution for serialization since its release in 2015. It supports many formats beyond JSON. Alternately, you might encounter `json-rust` or `simd-json`, which offer different performance trade-offs. A critical implementation detail to understand is that `serde` deserialization requires data structures to be known at compile-time, which is not the case with more dynamic languages like JavaScript.

## See Also:

- Serde's official documentation provides a comprehensive guide: [Serde Documentation](https://serde.rs)
- The underlying `serde_json` crate details: [serde_json Crate](https://docs.rs/serde_json)
- More about JSON itself: [Introducing JSON](https://www.json.org/json-en.html)
- For async programming with JSON, `tokio` and `async-std` are often used in tandem with `serde_json`.