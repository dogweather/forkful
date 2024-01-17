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

## What & Why?

JSON (JavaScript Object Notation) is a popular data format used by programmers for storing and exchanging data between different applications. It is a lightweight and readable format, making it easy for humans to understand and write, while also being easily processed by computers. Programmers use JSON because it allows for efficient data transfer and provides a structured way to organize and store data.

## How to:

Working with JSON in Rust is made easy with the help of the serde and serde_json crates. Follow these steps to get started:

1. Install the serde and serde_json crates by adding them to your ```Cargo.toml``` file.
2. Import the ```serde``` and ```serde_json``` crates into your code using the ```use``` keyword.
3. To serialize a Rust data structure into JSON, use the ```serde_json::to_string()``` function. This will return a string with the JSON representation of your data.
4. To deserialize a JSON string into a Rust data structure, use the ```serde_json::from_str()``` function. This will return a ```serde_json::Result``` enum, with the parsed data or an error if the string is not valid JSON.

```
use serde::{Serialize, Deserialize};
use serde_json;

// Serializing into JSON
#[derive(Serialize)]
struct Student {
    name: String,
    age: u8,
    grade: u8,
}
let john = Student {
    name: "John".to_string(),
    age: 16,
    grade: 11,
};
let json_string = serde_json::to_string(&john).unwrap(); // Serialize the `john` struct into a JSON string

// Deserializing from JSON
#[derive(Deserialize)]
struct Student {
    name: String,
    age: u8,
    grade: u8,
}
let json_string = r#"{"name":"John","age":16,"grade":11}"#; // JSON string
let john: Student = serde_json::from_str(json_string).unwrap(); // Deserialize the JSON string into a `Student` struct
```

Sample output:

```
{"name":"John","age":16,"grade":11}
```

## Deep Dive:

JSON was first invented by Douglas Crockford in 2001 as a way to represent data in a simple and consistent format. It gained popularity due to its compatibility with JavaScript, which made it a preferred choice for web developers. However, it is now widely used in many other programming languages, including Rust.

There are alternative libraries for working with JSON in Rust, such as json and rust-csv. However, serde and serde_json are the most widely used and have a larger community, providing additional support and updates.

In Rust, JSON data is deserialized into the ```serde_json::Value``` enum, which has different variants for representing different types of data, such as strings, numbers, arrays, and objects. This allows for flexible handling of JSON data without needing to know the exact structure beforehand.

## See Also:

- [serde documentation](https://serde.rs/)
- [serde_json documentation](https://docs.serde.rs/serde_json/)
- [An introduction to working with JSON in Rust](https://medium.com/@kbknapp/json-de-serialization-with-rust-1-0-67924f9b8c1)