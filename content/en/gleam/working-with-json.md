---
title:                "Working with JSON"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

Working with JSON (JavaScript Object Notation) means handling data in a ubiquitous text format that's easy for humans and machines to understand. Programmers do it because JSON is king for storing and transmitting structured data, especially in web applications.

## How to:

Here's how to handle JSON in Gleam by encoding and decoding data. You'll need the `gleam/json` package, so get that first.

```gleam
import gleam/json

// Define a type
pub type Person {
  Person(name: String, age: Int)
}

// Encode to JSON
pub fn encode_person(person: Person) -> json.Json {
  case person {
    Person(name, age) -> 
      json.object([
        "name", json.string(name),
        "age", json.int(age)
      ])
  }
}
// Usage and sample output
let john = Person("John Doe", 30)
let json_john = encode_person(john)
json_john // {"name": "John Doe", "age": 30}

// Decode from JSON
pub fn decode_person(json: json.Json) -> Result(Person, Nil) {
  let Ok(json) = json.decode_pair() // Decode the JSON object
  let Ok(name) = json.field("name").map(json.decode_string)
  let Ok(age) = json.field("age").map(json.decode_int)
  person.Person(name, age)
}
// Usage and sample output
let decoded_person = decode_person(json_object("{\"name\": \"John Doe\", \"age\": 30}"))
decoded_person // Ok(Person("John Doe", 30))
```

## Deep Dive

JSON's been around since the early 2000s, replacing XML in many scenarios for its simplicity. Alternatives include YAML, XML, and BSON, among others, but JSON's ease-of-use keeps it at the fore. In Gleam, JSON handling leans on pattern matching and the `gleam/json` library's robust functions for a functional approach to encode and decode data structures.

## See Also

- Gleam's official JSON documentation: [https://hexdocs.pm/gleam_json](https://hexdocs.pm/gleam_json)
- An introduction to JSON: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
- Mozilla Developer Network's guide on JSON: [https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON)
