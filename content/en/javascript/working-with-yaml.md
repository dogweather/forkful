---
title:                "Working with yaml"
html_title:           "Javascript recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

# YAML: the Simple Way to Store and Share Data

## What & Why?
YAML (YAML Ain't Markup Language) is a lightweight, human-readable data serialization language commonly used by programmers to store and share data in a structured format. It's designed to be easy to read and write, making it a popular choice for configuration files, data exchange between applications, and even website content.

## How to:
To start working with YAML, you'll need a text editor or an IDE (Integrated Development Environment) that supports YAML. Let's take a look at some basic examples of how to use YAML in a Javascript program:

**Creating a YAML Object:**
```Javascript
var yamlObj = {
  name: "John Smith",
  age: 28,
  interests: ["coding", "gaming", "hiking"]
};
```
**Nested Objects:**
```Javascript
var nestedObj = {
  name: "Jane Doe",
  contact: {
    email: "jane@example.com",
    phone: "123-456-7890"
  }
};
```
**Array of Objects:**
```Javascript
var arrayObj = [
  {
    name: "Bob",
    age: 35
  },
  {
    name: "Alice",
    age: 29
  },
]
```
**Sample Output:**
```Javascript
console.log(yamlObj.name); // Outputs "John Smith"
console.log(nestedObj.contact.email); // Outputs "jane@example.com"
console.log(arrayObj[1].age); // Outputs 29
```

## Deep Dive:
YAML was first introduced in 2001 and is based on the concept of human readability over machine readability. It's similar to XML and JSON, but with a simpler syntax that focuses on the hierarchical structure of data. It's widely used in web development, particularly in frameworks like Ruby on Rails and Node.js. It also has a wide range of library support for different programming languages, making it a flexible and widely adopted solution for data serialization.

An alternative to YAML is JSON (JavaScript Object Notation), which is also used for data interchange. While YAML is more readable and allows for comments and complex data structures, JSON is more widely supported and has a simpler syntax. A programmer may choose one over the other depending on their specific needs and preferences.

When working with YAML in Javascript, there are some important things to keep in mind. Firstly, YAML is whitespace sensitive, so indentation is significant and must be consistent. It also supports various data types including strings, numbers, arrays, and nested objects. It's important to properly format your data and use quotes when necessary to avoid any unexpected errors.

## See Also:
To learn more about YAML and how to use it, check out these resources:
- Official YAML website: https://yaml.org/
- YAML tutorial on W3Schools: https://www.w3schools.io/file/yaml-tutorial/
- Comparison between YAML and JSON: https://stackify.com/yaml-vs-json/