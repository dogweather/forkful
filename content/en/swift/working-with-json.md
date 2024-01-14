---
title:                "Swift recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON is a popular data format used in many web and mobile applications due to its flexibility and simplicity. Being able to work with JSON in Swift can greatly enhance your programming skills and allow you to create more dynamic and interactive apps.

## How To

To start working with JSON in Swift, we first need to import the Foundation framework. This will give us access to the `JSONSerialization` class which allows us to easily convert between JSON data and Swift objects.

```Swift
import Foundation

let jsonString = """
{
    "name": "John",
    "age": 30,
    "hobbies": ["programming", "reading", "hiking"]
}
"""

guard let jsonData = jsonString.data(using: .utf8) else {
    print("Invalid JSON string")
    return
}

do {
    let jsonObject = try JSONSerialization.jsonObject(with: jsonData, options: [])
    
    if let person = jsonObject as? [String: Any],
        let name = person["name"] as? String,
        let age = person["age"] as? Int,
        let hobbies = person["hobbies"] as? [String] {
        print("Name: \(name)")
        print("Age: \(age)")
        print("Hobbies: \(hobbies)")
    } else {
        print("Invalid JSON format")
    }
} catch {
    print("Error parsing JSON: \(error)")
}
```

The above example shows how we can use `JSONSerialization` to convert a JSON string into a Swift dictionary. We can then access the values from the dictionary using the appropriate types. This allows us to easily work with JSON data in our Swift code.

## Deep Dive

Working with JSON in Swift also gives us the ability to handle more complex data structures, such as nested objects and arrays. We can use the `JSONDecoder` class to decode JSON data directly into our own custom model objects.

```Swift
struct User: Codable {
    let name: String
    let age: Int
    let hobbies: [String]
}

do {
    let user = try JSONDecoder().decode(User.self, from: jsonData)
    print("Name: \(user.name)")
    print("Age: \(user.age)")
    print("Hobbies: \(user.hobbies)")
} catch {
    print("Error decoding JSON: \(error)")
}
```

By conforming to the `Codable` protocol and specifying the key-value mapping in our model object, we can easily convert JSON data into Swift objects. This can greatly simplify the process of parsing and working with complex JSON data.

## See Also

- [Apple Developer Documentation on JSONSerialization](https://developer.apple.com/documentation/foundation/jsonserialization)
- [Apple Developer Documentation on Codable](https://developer.apple.com/documentation/swift/codable)
- [Hacking with Swift - Working with JSON in Swift](https://www.hackingwithswift.com/read/7/2/working-with-json-in-swift)