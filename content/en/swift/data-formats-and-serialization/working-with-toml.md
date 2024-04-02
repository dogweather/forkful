---
date: 2024-01-25 03:39:30.893425-07:00
description: "TOML (Tom's Obvious, Minimal Language) is a data serialization format\
  \ that\u2019s easy to read due to its clear semantics. Programmers use TOML for\u2026"
lastmod: '2024-03-13T22:45:00.416696-06:00'
model: gpt-4-1106-preview
summary: "TOML (Tom's Obvious, Minimal Language) is a data serialization format that\u2019\
  s easy to read due to its clear semantics. Programmers use TOML for\u2026"
title: Working with TOML
weight: 39
---

## What & Why?
TOML (Tom's Obvious, Minimal Language) is a data serialization format that’s easy to read due to its clear semantics. Programmers use TOML for configuration files where readability by humans and easy parsing by machines are key.

## How to:
To start, you need a TOML parser. Swift doesn't have a built-in one, so let's use `TOMLDecoder`. Install it via Swift Package Manager and then serialize and deserialize TOML with ease.

```Swift
import TOMLDecoder

let tomlString = """
title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("Title: \(config.title), Owner: \(config.owner.name), DOB: \(config.owner.dob)")
    } catch {
        print("Error parsing TOML: \(error)")
    }
}
```

This code outputs:
```
Title: TOML Example, Owner: Tom Preston-Werner, DOB: 1979-05-27 07:32:00 +0000
```

## Deep Dive
TOML was designed by Tom Preston-Werner, GitHub’s co-founder, as a more human-friendly alternative to formats like JSON or YAML. It aims for clarity, reducing the chances of misinterpretation by a human or machine. As for alternatives, YAML and JSON are the usual suspects, with YAML skewed towards human readability and JSON as the simpler machine-friendly option. When working with TOML in Swift, we don't have a native parser. However, third-party libraries like `TOMLDecoder` facilitate easy conversion between TOML strings and Swift types, specifically via `Codable` protocols introduced in Swift 4 that streamlined serialization.

## See Also
- The TOML standard: https://toml.io
- GitHub for `TOMLDecoder`: https://github.com/dduan/TOMLDecoder
- Swift Documentation on `Codable`: https://developer.apple.com/documentation/swift/codable
- Comparison of data serialization formats: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
