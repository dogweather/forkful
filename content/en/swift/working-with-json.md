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

## Why

If you're a Swift developer, chances are you've come across JSON in your line of work. JSON, or JavaScript Object Notation, is a popular data interchange format that allows for easy transfer of data between platforms and applications. It's commonly used in web development, mobile app development, and data integration projects.

## How To

To work with JSON in Swift, you'll need to import the `Foundation` framework, which contains the `JSONSerialization` class. This class allows you to convert JSON to and from Swift data types such as `Dictionary` and `Array`.

Let's take a look at an example of parsing JSON data in Swift:

```Swift
// Sample JSON string
let jsonString = """
{
    "firstName": "John",
    "lastName": "Doe",
    "age": 30,
    "hobbies": ["reading", "hiking", "cooking"]
}
"""

// Convert JSON string to data
let jsonData = jsonString.data(using: .utf8)!

do {
    // Parse JSON data
    if let json = try JSONSerialization.jsonObject(with: jsonData) as? [String: Any] {
    
        // Extract data from JSON and assign to variables
        let firstName = json["firstName"]
        let lastName = json["lastName"]
        let age = json["age"]
        let hobbies = json["hobbies"] as? [String]
        
        // Print extracted data
        print(firstName ?? "Unknown")
        print(lastName ?? "Unknown")
        print(age ?? "Unknown")
        print(hobbies ?? "Unknown")
    }
} catch {
    print("Error parsing JSON: \(error)")
}
```

Output:
```
John
Doe
30
["reading", "hiking", "cooking"]
```

As you can see, we converted the JSON string into data and then used the `JSONSerialization` class to parse the data into a Swift `Dictionary`. From there, we were able to access and print out the data as desired.

## Deep Dive

If you're interested in diving deeper into working with JSON in Swift, there are a few key things to keep in mind. First, it's important to properly format your JSON data. This means using double quotes for keys and string values, and using square brackets for arrays.

Another useful feature in Swift for working with JSON is the `Codable` protocol. This protocol allows for easy encoding and decoding of Swift objects to and from JSON. Simply conform your objects to `Codable` and use the `JSONEncoder` and `JSONDecoder` classes to convert back and forth between JSON and your custom objects.

## See Also

- [Apple Developer Documentation: JSONSerialization](https://developer.apple.com/documentation/foundation/jsonserialization)
- [Ray Wenderlich: Working with JSON in Swift](https://www.raywenderlich.com/1124570-working-with-json-in-swift)
- [Swift.org: Decodable](https://swift.org/documentation/api-design-guidelines/#decoding)