---
title:                "Working with yaml"
html_title:           "Kotlin recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

# What & Why?
Working with YAML is a common practice among programmers as it allows for the better organization and storage of data. It is a data serialization language that uses a human-readable format, making it easier for developers to read and modify compared to other data formats. YAML is used for various purposes such as configuration files, data interchange, and more.

# How to:
To start using YAML in Kotlin, the `yaml` library must be added as a dependency in your project. Then, import the library using `import org.yaml.snakeyaml.Yaml`.

To parse a YAML file, simply use the `load()` method and pass in the file's input stream as a parameter. This will return a `LinkedHashMap` object that contains the data from the YAML file. Here's an example:

```Kotlin 
val yaml = Yaml()
val data = yaml.load(inputStream)
println(data)
```

To create a YAML file, use the `dump()` method and pass in a `LinkedHashMap` object with the desired data. This will return a `String` containing the YAML data. Here's an example:

```Kotlin
val yaml = Yaml()
val data = linkedMapOf(
    "name" to "John",
    "age" to 25,
    "city" to "New York"
)
val yamlData = yaml.dump(data)
println(yamlData)
```

The output for both code blocks would be:

```
{name=John, age=25, city=New York}
```

# Deep Dive:
Created in 2001, YAML stands for "YAML Ain't Markup Language" and was created by Clark Evans. It was designed as a human-readable data serialization language that is more readable and concise compared to XML and JSON. YAML is commonly used for configuration files and data interchange between systems.

An alternative to YAML is JSON, which is more widely used and has better support across different languages. However, YAML offers more functionality and is easier to read and maintain compared to JSON. 

The `yaml` library in Kotlin uses the SnakeYAML implementation, which follows the YAML 1.2 specification. This library is actively maintained and supports various YAML features such as anchors and aliases, flow collections, and custom tags.

# See Also:
- [YAML Official Website](https://yaml.org/)
- [SnakeYAML GitHub Repository](https://github.com/snakeyaml/)