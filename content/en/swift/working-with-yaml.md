---
title:                "Working with yaml"
html_title:           "Swift recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

---

# What & Why?

Working with YAML is all about organizing data in a human-readable format. It is a type of data serialization that allows programmers to store and transmit data in a format that is both easy to read and machine-readable. Many developers use YAML in their projects because it is a versatile and efficient way to manage complex data structures.

# How to:

Implementing YAML in Swift is relatively straightforward. Here are a few examples to get you started:

### 1. Create a YAML string:

```Swift
let yamlString = """
    name: John Doe
    age: 35
    occupation: Programmer
"""
```

### 2. Convert a YAML string to a dictionary:

```Swift
let yamlString = """
    name: John Doe
    age: 35
    occupation: Programmer
"""

let yamlDict = try! YAMLSerialization.object(withYAML: yamlString) as! [String: Any]
```

### 3. Convert a dictionary to YAML string:

```Swift
let person = ["name": "John Doe", "age": 35, "occupation": "Programmer"]

let yamlString = try! YAMLSerialization.yamlString(withObject: person)
```

# Deep Dive:

YAML was first created in 2001 as a more user-friendly alternative to XML, and has since become a popular choice for managing data in web applications. YAML stands for "YAML Ain't Markup Language" and is designed to be easy to read and write for both humans and machines.

An alternative to YAML is JSON, which is also a popular data serialization format. However, many developers prefer YAML because it allows for more human-friendly formatting, such as indentation, which can make complex data structures easier to manage.

Internally, the SwiftYAML library uses the LibYAML C library, making it fast and efficient at parsing YAML data. It also supports the ability to customize the way it handles data through the use of Codable protocols.

# See Also:

- [SwiftYAML GitHub page](https://github.com/behrang/YamlSwift)
- [YAML Specification](https://yaml.org/spec/1.2/spec.html)
- [Introduction to YAML](https://www.datacamp.com/community/tutorials/working-yaml-python)