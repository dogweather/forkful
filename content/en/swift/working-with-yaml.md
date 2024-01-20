---
title:                "Working with yaml"
html_title:           "Arduino recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, short for "YAML Ain't Markup Language", is a human-readable data serialization standard that we can use to configure files or data exchange. Programmers love YAML for its simplicity and readability, especially in configuration settings, CI/CD scripts, and container orchestration systems.

## How to:
Swift doesn't natively handle YAML, so we need to use a third-party library like Yams. First, add Yams to your `Package.swift`:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

Then, import Yams and use it to parse YAML into a Swift dictionary:

```swift
import Yams

let yamlString = """
name: John Doe
age: 34
languages:
  - Swift
  - Python
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
    }
} catch {
    print("Failed to parse YAML string.")
}

// Output:
// ["name": "John Doe", "age": 34, "languages": ["Swift", "Python"]]
```

If you want to generate YAML from Swift objects:

```swift
import Yams

let dictionary: [String: Any] = [
    "name": "Jane Smith",
    "age": 28,
    "languages": ["Java", "Kotlin"]
]

do {
    let yaml = try Yams.dump(object: dictionary)
    print(yaml)
} catch {
    print("Failed to convert dictionary to YAML.")
}

// Output:
// age: 28
// languages:
//   - Java
//   - Kotlin
// name: Jane Smith
```

## Deep Dive
YAML originated in 2001 as a human-friendly alternative to XML. It resembles JSON with less use of braces and better human readability. While JSON is a go-to for web APIs, YAML is preferred for configuration files. Alternatives include TOML and JSON5, but YAML's use of whitespace and the ability to comment lines make it desirable. With Yams, Swift approaches YAML processing with class mapping, offering a balance between script-like simplicity and type safety.

## See Also
- YAML official site for spec details: [https://yaml.org](https://yaml.org)
- Yams GitHub Repository: [https://github.com/jpsim/Yams](https://github.com/jpsim/Yams)
- Swift Package Manager Documentation: [https://swift.org/package-manager/](https://swift.org/package-manager/)