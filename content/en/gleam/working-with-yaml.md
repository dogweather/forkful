---
title:                "Working with yaml"
html_title:           "Gleam recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why
If you're working with multiple programming languages or need to store data in a human-readable format, YAML is a great option. It's a lightweight, easy-to-read language that makes organizing and sharing data a breeze.

## How To
To start using YAML in your Gleam project, first install the latest version of the official Gleam YAML library. Then, import it into your code with `import gleam/yaml`.

Next, let's take a look at a basic example of how to create and save a YAML file:

```Gleam
let my_data = [
  { "name":  "John", "age": 26 },
  { "name":  "Jane", "age": 32 }
]

let file = yaml.encode(my_data)
File.write("./sample.yaml", file)
```

In this code, we create a list of dictionaries containing some sample data. Then, we use the `encode` function from the YAML library to convert the data into YAML format. Finally, we use the `File.write` function to save the YAML data to a file.

To read and use data from a YAML file, we can use the `decode` function:

```Gleam
let data = File.read("./sample.yaml")
let decoded_data = yaml.decode(data)
```

The `decoded_data` variable will now contain the same list of dictionaries that we created earlier.

## Deep Dive
Now that you know the basics of creating and reading YAML files in Gleam, let's take a deeper dive into some advanced features.

First, it's important to note that Gleam's YAML library follows the YAML 1.2 standard. This means that you can take advantage of all the powerful features that come with the standard, such as anchors, aliases, and custom tags.

Additionally, the library provides functions for formatting, parsing, and validating YAML data. This allows for more control and flexibility when working with YAML.

And finally, the library also supports converting between YAML and JSON. This can come in handy when working with other programming languages that may not have native support for YAML.

## See Also
- [Official Gleam YAML Library](https://github.com/gleam-lang/yaml)
- [YAML Specification](https://yaml.org/spec/1.2/spec.html)
- [JSON to YAML Converter](https://www.json2yaml.com/)