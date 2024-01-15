---
title:                "Working with yaml"
html_title:           "Ruby recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

Working with YAML can make your life as a programmer much easier. It allows you to easily store and access data in a human-readable format, making it ideal for configuration files, data storage, and more.

## How To

To start using YAML in your Ruby projects, first make sure you have the `yaml` gem installed. Then, you can use the `YAML.load_file` method to load a YAML file:

```Ruby
require 'yaml'

data = YAML.load_file('config.yml')
```

You can also use the `YAML.dump` method to convert a Ruby object into a YAML string:

```Ruby
hash = { key1: "value1", key2: "value2" }

yaml_string = YAML.dump(hash)
```

And to save a YAML file, you can use the `File.write` method with the `YAML.dump` method:

```Ruby
hash = { key1: "value1", key2: "value2" }

File.write("data.yml", YAML.dump(hash))
```

## Deep Dive

YAML stands for "YAML Ain't Markup Language" and is a human-readable data serialization language. It uses indentation and colons to represent data structures, making it easy to read and write. 

YAML also supports various data types such as strings, numbers, booleans, arrays, and hashes. These data types can be nested and even referenced within the same YAML document. 

Additionally, YAML allows for comments within the document, making it a great choice for configuration files that may require explanations or notes. 

## See Also

- [YAML specification](https://yaml.org/spec/1.2/spec.html)
- [YAML gem documentation](https://rubydoc.info/stdlib/yaml/index)
- [Ruby official website](https://www.ruby-lang.org/)