---
title:                "Swift recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why

YAML is a popular data serialization format commonly used in modern software development. It is human-readable, easy to learn, and versatile, making it a great choice for storing configuration or data in applications. In this blog post, we will explore the basics of YAML and how to work with it in Swift.

## How To

To start working with YAML in Swift, we will need to add a dependency to our project. The most popular library for handling YAML in Swift is Yams, which can be easily integrated using Swift Package Manager.

```Swift
.package(url: "https://github.com/jpsim/Yams.git", from: "3.0.0")
```

Once the library is added, we can start parsing YAML data by creating a Yams parser instance and passing in the YAML string:

```Swift
import Yams

let yamlString = """
fruits:
  - apple
  - banana
  - orange
"""
let parser = Parser()
let yamlData = try parser.load(yaml: yamlString)
```

Yams will automatically convert the YAML data into a Swift dictionary. We can then access the values using their corresponding keys:

```Swift
guard let fruits = yamlData["fruits"] as? [String] else {
    print("Error parsing YAML data")
    return
}

// fruits = ["apple", "banana", "orange"]
```

We can also convert a Swift dictionary into YAML data using the Yams library:

```Swift
let dictionary: [String: Any] = ["name": "John", "age": 25, "hobbies": ["reading", "coding"]]
let yamlString = try Yams.dump(object: dictionary)
```

The resulting YAML string will look like this:

```Swift
name: John
age: 25
hobbies:
    - reading
    - coding
```

## Deep Dive

YAML supports a wide range of data types, including strings, integers, booleans, arrays, and dictionaries. It also allows for comments, making it easier for developers to understand the data structure. One important thing to note is that indentation is significant in YAML, as it determines the hierarchy of the data.

YAML also supports anchors and aliases, which can be useful for referencing values in different parts of a YAML document. This allows for reusing data and improving the readability of YAML files.

## See Also

- [Official YAML site](https://yaml.org/)
- [Yams GitHub repo](https://github.com/jpsim/Yams)
- [Swift Package Manager](https://swift.org/package-manager/)