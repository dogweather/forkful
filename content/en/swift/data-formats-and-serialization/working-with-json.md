---
date: 2024-01-19
description: 'How to: Swift makes JSON parsing straightforward with the `Codable`
  protocol. Here''s how you decode JSON to a Swift object.'
lastmod: '2024-03-13T22:45:00.414999-06:00'
model: unknown
summary: Swift makes JSON parsing straightforward with the `Codable` protocol.
title: Working with JSON
weight: 38
---

## How to:
Swift makes JSON parsing straightforward with the `Codable` protocol. Here's how you decode JSON to a Swift object:

```Swift
import Foundation

// Define a model that conforms to Codable
struct User: Codable {
    var name: String
    var age: Int
}

// JSON string
let jsonString = """
{
    "name": "John Doe",
    "age": 30
}
"""

// Convert JSON string to Data
if let jsonData = jsonString.data(using: .utf8) {
    // Decode JSON data to User object
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("Name: \(user.name), Age: \(user.age)")
    } catch {
        print("Error decoding JSON: \(error)")
    }
}
```

Sample output:
```
Name: John Doe, Age: 30
```

## Deep Dive
JSON (JavaScript Object Notation) has been widely adopted since the early 2000s, after Douglas Crockford specified it. It replaced XML for many use cases due to its simpler syntax and better performance. While Swift's `Codable` is the go-to for JSON, alternatives like `JSONSerialization` exist for when dealing with non-Codable-compliant types. Under the hood, `Codable` abstracts away the lower-level parsing and makes serialization/deserialization seamless.

## See Also
- Explore more on JSON and Swift in the official Swift blog: [Swift.org](https://swift.org/blog/)
- Check out the `Codable` documentation: [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- For complex JSON structures, consider third-party libraries such as SwiftyJSON available on [GitHub](https://github.com/SwiftyJSON/SwiftyJSON).
