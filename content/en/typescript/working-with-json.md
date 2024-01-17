---
title:                "Working with json"
html_title:           "TypeScript recipe: Working with json"
simple_title:         "Working with json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

Working with JSON (JavaScript Object Notation) is the act of manipulating and accessing data in a format that is easy for humans to read and write, and for machines to parse and generate. Programmers often work with JSON because it is a lightweight and efficient way to store and transmit data between applications and servers.

## How to:

Coding examples and sample output:

```TypeScript
// Creating a JSON object
let person = {
  "name": "John",
  "age": 25,
  "location": "New York"
}

// Accessing values in JSON
console.log(person.name); // Output: John
console.log(person.age); // Output: 25
console.log(person.location); // Output: New York

// Adding a new property to the object
person.job = "Software Developer";
console.log(person); // Output: { "name": "John", "age": 25, "location": "New York", "job": "Software Developer" }
```

## Deep Dive:

JSON was first introduced in 1999 as a simplified alternative to XML. It quickly gained popularity due to its lightweight nature, making it easy to parse and ideal for use in web applications. It is also supported by many programming languages, including TypeScript. When working with JSON, there are alternative formats such as YAML and XML, but JSON remains the standard for data interchange.

In TypeScript, JSON data can be easily converted to objects using the `JSON.parse()` method, and objects can be converted to JSON strings using the `JSON.stringify()` method. This makes it easy to send and receive data between a client and a server. JSON also supports arrays, making it a versatile data format.

## See Also:

- [JSON tutorial on W3Schools](https://www.w3schools.com/js/js_json_intro.asp)
- [JSON on MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)