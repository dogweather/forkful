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

**What & Why?**

Working with JSON in programming is all about efficiently handling data. JSON (JavaScript Object Notation) is a lightweight and easy-to-read format for storing and transmitting data between different systems. Programmers use it to transfer data over the internet, store data in databases, and manipulate data within their programs.

**How to:**

The Gleam language has built-in support for dealing with JSON data. Here's a quick example of how to encode and decode JSON using Gleam:

```gleam
// Encode a JSON object
let user = Json.encode({ name: "John", age: 30, is_active: true })

// Decode JSON into a Gleam record
let user_data = Json.decode(user)
```

The ```encode()``` function takes in a Gleam record and returns a JSON string. The ```decode()``` function takes in a JSON string and converts it into a Gleam record. It's as simple as that!

**Deep Dive:**

JSON was created in 2002 as a simpler alternative to XML for data transmission. It quickly gained popularity due to its simplicity and flexibility, and is now the preferred format for data exchange in many applications.

While there are other data formats like XML and YAML, JSON is still widely used because of its easy integration with web-based systems and its ability to represent complex data structures in a human-readable format. Other programming languages like JavaScript and Python also have built-in support for JSON, making it a popular choice for data communication.

Internally, Gleam uses the popular Jiffy library for encoding and decoding JSON. Jiffy is written in Erlang and is known for its fast performance and error handling capabilities.

**See Also:**

- [Official Gleam documentation for handling JSON](https://gleam.run/book/tutorials_and_guides/json.html)
- [Introduction to JSON by W3Schools](https://www.w3schools.com/js/js_json_intro.asp)
- [Jiffy library for working with JSON in Erlang](https://github.com/davisp/jiffy)