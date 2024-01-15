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

## Why

JSON (JavaScript Object Notation) is a popular data-interchange format used for storing and transmitting data. It is widely used in web development and is essential for creating flexible and dynamic applications. By learning how to work with JSON in TypeScript, you can effectively manipulate and manage data in your projects.

## How To

To work with JSON in TypeScript, follow these simple steps:

### 1. Import the JSON library

To start working with JSON, you need to first import the `JSON` library into your project. This library provides useful methods for parsing and stringifying data in JSON format.

```
import { JSON } from 'typescript';
```

### 2. Parsing JSON data

To parse JSON data from a string, use the `parse()` method. This will take in a string containing JSON data and convert it into a TypeScript object.

```
let jsonString = '{"name": "John", "age": 30, "hobbies": ["gaming", "reading"]}';
let person = JSON.parse(jsonString);

console.log(person.name); // Output: John
console.log(person.hobbies[0]); // Output: gaming
```

### 3. Stringifying JSON data

To convert a TypeScript object into a JSON string, use the `stringify()` method. This will take in an object and convert it into a JSON string.

```
let person = { name: "John", age: 30, hobbies: ["gaming", "reading"] };
let jsonString = JSON.stringify(person);

console.log(jsonString); // Output: {"name": "John", "age": 30, "hobbies": ["gaming", "reading"]}
```

### 4. Manipulating JSON data

You can also manipulate JSON data in TypeScript by accessing and modifying the properties of the converted object.

```
let jsonString = '{"language": "TypeScript", "version": "4.4.3"}';
let info = JSON.parse(jsonString);
info.version = "4.4.4"

console.log(info); //Output: {"language": "TypeScript", "version": "4.4.4"}
```

## Deep Dive

When working with larger and complex JSON data, you can use interfaces to define the structure of the data and assign proper types to the properties. This will ensure that your data remains consistent and error-free when accessing or modifying it.

```
interface User {
    name: string;
    age: number;
    hobbies: string[];
}

let jsonString = '{"name": "John", "age": 30, "hobbies": ["gaming", "reading"]}';
let user: User = JSON.parse(jsonString);

console.log(user.name); // Output: John
console.log(user.age); // Output: 30
user.hobbies.push("coding");

console.log(user); // Output: {"name": "John", "age": 30, "hobbies": ["gaming", "reading", "coding"]}
```

## See Also

- [Introduction to JSON](https://www.json.org/json-en.html)
- [TypeScript official documentation on JSON](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-9.html#support-for-json-ts)
- [Working with JSON in Node.js](https://www.sitepoint.com/python-json-serialize-decode-python/)