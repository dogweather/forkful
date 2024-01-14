---
title:                "Rust recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

Have you ever struggled with organizing and managing configuration files in your projects? Look no further because YAML is here to help! YAML is a human-readable data serialization language that is commonly used for storing and organizing configuration data. It is simple to use and understand, making it a popular choice for developers.

## How To

To get started with YAML in Rust, we first need to add the `yaml` crate to our `Cargo.toml` file:

```Rust
[dependencies]
yaml = "0.4.5"
```

Next, we can use the `serde_yaml` crate to easily serialize and deserialize YAML data. Let's take a look at an example of how to read a YAML file and print its contents:

```Rust
use std::fs::File;
use serde_yaml::Value;

fn main() {
    // Open YAML file
    let file = File::open("config.yaml").expect("Unable to open file.");

    // Deserialize YAML data into a value
    let value: Value = serde_yaml::from_reader(file).expect("Unable to parse YAML.");

    // Print the value
    println!("{:#?}", value);
}
```

This code reads a `config.yaml` file and deserializes it into a `Value` that we can then print out. The output will look something like this:

```
Mapping(
    {
        String(
            "name",
        ): String(
            "John",
        ),
        String(
            "age",
        ): Integer(
            25,
        ),
        String(
            "hobbies",
        ): Sequence(
            [
                String(
                    "coding",
                ),
                String(
                    "reading",
                ),
                String(
                    "gaming",
                ),
            ],
        ),
    },
)
```

As you can see, YAML data is represented in Rust as a `Value` enum, which can contain different types of data such as strings, integers, and sequences. We can use pattern matching to access specific data within the `Value`.

Similarly, if we want to create a YAML file from scratch, we can use serde to serialize a Rust data structure into YAML format:

```Rust
use std::fs::File;
use serde_yaml::to_writer;

// Create a struct to serialize
struct Person {
    name: String,
    age: u8,
    hobbies: Vec<String>,
}

let person = Person {
    name: "Jane".to_owned(),
    age: 30,
    hobbies: vec!["painting".to_owned(), "hiking".to_owned()],
};

// Serialize struct into YAML
let file = File::create("person.yaml").expect("Unable to create file.");
to_writer(file, &person).expect("Unable to serialize person.");
```

This will create a `person.yaml` file in the current directory with the following contents:

```yaml
name: Jane
age: 30
hobbies:
    - painting
    - hiking
```

## Deep Dive

While YAML may seem similar to other data serialization formats like JSON, it offers some unique features that make it a great choice for configuration files. YAML has support for comments, making it easier to document and explain complex configurations. It also supports anchors and aliases, which let you reference data from one part of the file to another, reducing repetition and making it easier to maintain.

Additionally, YAML allows for multi-line strings, making it easier to write and read long blocks of text. It also supports more complex data types such as dates and timestamps, making it versatile for different types of data.

For a complete understanding of how to use YAML in Rust, check out the official documentation for [`serde_yaml`](https://docs.rs/serde_yaml/).

## See Also

- [serde-yaml - GitHub](https://github.com/dtolnay/serde-yaml)
- [Introduction to YAML - YAML.org](https://yaml.org/intro.html)
- [Intro to Rust Serde - Medium](https://medium.com/@joshuajrh/intro-to-rust-serde-22b305e64d9b)