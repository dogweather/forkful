---
title:                "Working with yaml"
html_title:           "Rust recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

YAML, or "YAML Ain't Markup Language", is a popular data serialization format commonly used for configuration files and data storage. Working with YAML allows for more human-readable and user-friendly data representation compared to other formats such as JSON and XML. Additionally, Rust's strong type system and error handling make it a great language for working with YAML.

## How To

To start working with YAML in Rust, we first need to add the `serde_yaml` crate to our project's dependencies in the `Cargo.toml` file:

```
[dependencies]
serde_yaml = "0.8.14"
```

Next, we need to import the `serde` and `serde_yaml` crates into our code:

```
use serde::{Deserialize, Serialize};
use serde_yaml;
```

Now, we can use the `serde_yaml` crate to parse a YAML file into a Rust struct, and vice versa. Let's take a look at an example where we have a `Config` struct representing some application configuration:

```
#[derive(Debug, Serialize, Deserialize)]
struct Config {
    name: String,
    port: i32,
    database_url: String,
}
```

Given a YAML file named `config.yaml` with the following content:

```
name: MyApp
port: 8080
database_url: postgres://user:pass@localhost/myapp_database
```

We can use the `serde_yaml` crate to deserialize the YAML into our `Config` struct:

```
fn main() {
    let file = File::open("config.yaml").expect("Error opening file");
    let config: Config = serde_yaml::from_reader(file).expect("Error parsing YAML");
    println!("{:?}", config);
}
```

We can also go the other way around and serialize our `Config` struct into a YAML file:

```
fn main() {
    let config = Config {
        name: "MyApp".to_string(),
        port: 8080,
        database_url: "postgres://user:pass@localhost/myapp_database".to_string(),
    };
    let mut file = File::create("config.yaml").expect("Error creating file");
    serde_yaml::to_writer(&mut file, &config).expect("Error serializing YAML");
}
```

This will produce a `config.yaml` file with the same content as the example above.

## Deep Dive

The `serde_yaml` crate provides a lot of useful options for customizing how YAML is parsed and serialized. For example, if we wanted to rename a field in our `Config` struct to match a different key in the YAML file, we can use the `#[serde(rename = "field_name")]` attribute:

```
#[derive(Debug, Serialize, Deserialize)]
struct Config {
    #[serde(rename = "project_name")]
    name: String,
    port: i32,
    database_url: String,
}
```

Additionally, the `deserialize_with` attribute allows us to provide a function that will handle the deserialization of a particular field. This is useful for cases where we need to convert a string value into a different type:

```
#[derive(Debug, Serialize, Deserialize)]
struct Config {
    name: String,
    port: i32,
    #[serde(deserialize_with = "deserialize_database_url")]
    database_url: url::Url,
}

fn deserialize_database_url<'de, D>(deserializer: D) -> Result<url::Url, D::Error>
where
    D: Deserializer<'de>,
{
    let s: String = Deserialize::deserialize(deserializer)?;
    Url::parse(&s).map_err(de::Error::custom)
}
```

For more information on customizing the serialization and deserialization process, check out [the official serde documentation](https://serde.rs/).

## See Also

Here are some resources for further learning about working with YAML in Rust:

- [serde_yaml crate documentation](https://docs.rs/serde_yaml/)
- [Official rust-serde GitHub repository](https://github.com/serde-rs/serde)
- [Rust Cookbook: Parsing YAML files with serde_yaml](https://rust-lang-nursery.github.io/rust-cookbook/serialization/yaml.html)