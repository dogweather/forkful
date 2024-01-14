---
title:                "Gleam recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Why 
JSON, or JavaScript Object Notation, has become the go-to data interchange format for web development due to its simplicity and versatility. Understanding how to work with JSON in Gleam can greatly enhance your development skills and make your work more efficient.

## How To
Working with JSON in Gleam is straightforward and can be done using the `gleam/json` library. Let's take a look at some coding examples to see how it works.

First, we will need to import the `json` module from the `gleam/json` library. This module provides functions for encoding and decoding JSON.

```Gleam
import gleam/json/json

// Define a type for our JSON data 
type User {
    name: String,
    age: Int,
    hobby: String
}

// Encode our data into JSON format 
let user = User{name: "John", age: 25, hobby: "Coding"}
let encoded_user = json.encode(user)
// Output: {"name":"John","age":25,"hobby":"Coding"}

// Decode a JSON string into a specific type 
let json_string = "{\"name\":\"Mary\",\"age\":30,\"hobby\":\"Gardening\"}"
let decoded_user = json.decode(json_string, User)
// Output: User{name: "Mary", age: 30, hobby: "Gardening"}
```

As we can see, using the `gleam/json` library, we can easily encode and decode data into JSON format. This makes it easy to send and receive data from external services and APIs.

## Deep Dive
In Gleam, working with JSON is made even easier with the use of record types. Record types allow us to define a structure for our data, making it clearer and more organized.

Additionally, in the `json` module, there are also functions for handling more complex JSON structures, such as arrays and nested objects. These functions make it easier to access and manipulate specific data within a JSON string.

## See Also
For more information on working with JSON in Gleam, check out the official documentation: 
- [Gleam - JSON module](https://gleam.run/modules/gleam/json/latest/api/)
- [Using JSON in Gleam - Guide](https://gleam.run/articles/using-json-in-gleam.html)