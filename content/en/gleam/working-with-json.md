---
title:                "Working with json"
html_title:           "Gleam recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON (JavaScript Object Notation) is a popular data format used for storing and exchanging data between different applications. It is a lightweight and human-readable format, making it great for data transmission. Working with JSON in a programming language like Gleam allows you to easily manipulate and extract data, making it a valuable skill for any developer.

## How To

To work with JSON in Gleam, you will first need to create a new Gleam project or open an existing one. Then follow these simple steps:

1. Import the JSON library by adding `json = "0.18.0"` to the `deps` section of your `gleam.toml` file.
2. In your code, use `import json` to import the JSON library.
3. To decode a JSON string into a Gleam type, use the `json.parse` function.
4. To encode a Gleam type into a JSON string, use the `json.encode` function.

Here's an example of decoding a JSON string and accessing its fields:

```Gleam
import json
import gleam/expect

let data = """{
  "name": "John",
  "age": 30
}"""

let decoded = json.parse(data)
let name = expect.Ok(decoded.name) // "John"
let age = expect.Ok(decoded.age) // 30
```

And here's an example of encoding a Gleam type into a JSON string:

```Gleam
import json

type Person {
  name: String,
  age: Int
}

let john = Person("John", 30)
let encoded = json.encode(john) // "{\"name\":\"John\",\"age\":30}"
```

## Deep Dive

There are a few things to keep in mind when working with JSON in Gleam:

- Gleam's type system is strict, which means it will not automatically convert between different types. This means you will need to use helper functions such as `String.to_int` and `Int.to_string` when working with JSON data.
- When decoding JSON strings, the `json.parse` function will return a result type, `Result(Error, T)`, where `T` is the decoded Gleam type. You can use the `expect` module to handle error cases and extract the decoded data.
- When encoding Gleam types into JSON strings, the `json.encode` function will take any type that implements the `json.Encode` protocol. This includes built-in types like strings, integers, and lists, as well as custom types that you define.

Now that you have a basic understanding of working with JSON in Gleam, you can explore more complex scenarios and see how you can leverage the `json` library to make your code more efficient and readable.

## See Also

- Official Gleam documentation on working with JSON: https://gleam.run/book/tour/json.html
- The `json` library on GitHub: https://github.com/gleam-lang/gleam/blob/master/lib/json/README.md