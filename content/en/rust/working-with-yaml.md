---
title:                "Working with YAML"
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

Working with YAML means dealing with data in the "YAML Ain't Markup Language" format—a human-friendly data serialization standard. Programmers use it for config files, data storage, or anywhere they need easily readable and writable structured data.

## How to:

To parse and generate YAML in Rust, we use the `serde_yaml` crate, which leans on `serde` for serialization/deserialization.

First, add dependencies to your `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

Now let's serialize a Rust struct to YAML:

```rust
use serde::{Serialize, Deserialize};
use serde_yaml;

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    debug: bool,
    environment: String,
    port: u16,
}

fn main() -> serde_yaml::Result<()> {
    let config = Config {
        debug: true,
        environment: "development".to_string(),
        port: 8080,
    };

    // Serialize to YAML
    let yaml_string = serde_yaml::to_string(&config)?;
    println!("{}", yaml_string);
    // Output:
    // ---
    // debug: true
    // environment: "development"
    // port: 8080

    Ok(())
}
```

To deserialize YAML into a Rust struct:

```rust
fn main() -> serde_yaml::Result<()> {
    let yaml_string = r#"
    debug: true
    environment: "development"
    port: 8080
    "#;

    let config: Config = serde_yaml::from_str(&yaml_string)?;
    println!("{:?}", config);
    // Output:
    // Config { debug: true, environment: "development", port: 8080 }

    Ok(())
}
```

## Deep Dive

YAML started in 2001 as a user-friendly alternative to XML. Unlike JSON, YAML supports comments and is less noisy, making it a favorite for config files. Rust's `serde_yaml` leverages `serde` for data conversion, ensuring high compatibility and flexibility. While `serde_json` is more commonly used due to JSON's ubiquity in APIs, `serde_yaml` shines for local config and data files. It's worth noting that overly complex YAML features are rarely used and sometimes discouraged due to potential parsing issues.

## See Also

For further reading and more complex use cases:

- Serde's official documentation: https://serde.rs/
- Serde YAML crate documentation: https://docs.rs/serde_yaml/latest/serde_yaml/
- YAML official specification: https://yaml.org/spec/1.2/spec.html
- The Rust Programming Language book: https://doc.rust-lang.org/book/
