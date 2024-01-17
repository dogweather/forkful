---
title:                "Working with json"
html_title:           "Swift recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Working with JSON is a commonly used method for transferring and storing data in a lightweight, human-readable format. It stands for JavaScript Object Notation and is widely used in web development and mobile app development. Programmers use JSON for its simplicity, flexibility, and support across multiple platforms.

## How to:
To work with JSON in Swift, you can use the built-in JSONSerialization class. First, you will need to convert your data into a Data object. Then, you can use the JSONSerialization class to serialize the data into a JSON format. Here's an example:

```Swift
let jsonData = try JSONSerialization.data(withJSONObject: data)
```

This code snippet converts the data given into a JSON format and stores it in a Data object called jsonData. Alternatively, you can also use the Codable protocol to convert between JSON and Swift structures. Here's an example:

```Swift
struct Person: Codable {
    var name: String
    var age: Int
}

let json = """
    {
        "name": "John Doe",
        "age": 25
    }
"""

let jsonData = json.data(using: .utf8)!

let person = try JSONDecoder().decode(Person.self, from: jsonData)
```

In this example, we define a Person struct that conforms to the Codable protocol, which allows us to easily convert JSON data into a Person object using the JSONDecoder.

## Deep Dive:
JSON was created in 2001 by Douglas Crockford as an alternative to XML for data exchange. It has gained popularity due to its simplicity, readability, and compatibility with JavaScript, making it ideal for web development. There are alternative ways to work with JSON in Swift, such as using third-party libraries like SwiftyJSON or ObjectMapper, which offer additional features for handling and mapping JSON data.

When working with a large amount of data, using the Codable protocol can help improve performance compared to using the JSONSerialization class. This is because the Codable protocol leverages the compiler's knowledge of your data structure, resulting in faster data parsing. Additionally, if you need to convert your data into different formats, Codable allows you to define custom encoding and decoding logic to suit your needs.

## See Also:
- [Apple's JSONSerialization Class Documentation](https://developer.apple.com/documentation/foundation/jsonserialization)
- [Swift Codable Protocol Documentation](https://developer.apple.com/documentation/swift/codable)
- [SwiftyJSON Library](https://github.com/SwiftyJSON/SwiftyJSON)
- [ObjectMapper Library](https://github.com/tristanhimmelman/ObjectMapper)