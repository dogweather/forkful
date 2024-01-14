---
title:                "Gleam recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

##Why

Working with YAML in Gleam can improve the readability and maintainability of your code. YAML is a popular human-readable data serialization format that allows you to easily store and access data in a structured way.

##How To

To start working with YAML in Gleam, you will need to install the yaml package from the Gleam package registry. 

```
Gleam info add "yaml"
```

Next, you can import the yaml package in your Gleam module and use the `parse` function to convert a YAML string into a Gleam type.

```
import yaml 

let yaml_string = "name: John
age: 30"

let user: Result(yaml.Decode.YamlDecoderError, User) =
    yaml.parse(yaml_string, User.decode())

pub struct User(name: String, age: Int)

impl User {
    pub fn decode(decoder: yaml.Decode.Decoder) {
        yaml.Decode.field("name", yaml.Decode.string, self.name, decoder)
        yaml.Decode.field("age", yaml.Decode.int, self.age, decoder)
    }
}
```

You can then use pattern matching to handle the `Result` and access the data in your Gleam code.

```
case user {
    Ok(user) -> 
        // Do something with user
    Err(err) -> 
        // Handle error
}
```

##Deep Dive

In addition to parsing YAML strings, the yaml package also allows you to encode a Gleam type into a YAML string using the `encode` function. This can be useful for converting Gleam data into a format that other applications can easily read and understand.

The `decode` and `encode` functions are just two of the many functions available in the yaml package. For a complete list of functions and their usage, you can refer to the [official documentation](https://gleam.run/packages/yaml/). 

##See Also

- [Gleam package registry](https://gleam.run/packages/)
- [Official documentation for the yaml package](https://gleam.run/packages/yaml/)