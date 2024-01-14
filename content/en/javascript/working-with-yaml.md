---
title:                "Javascript recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why
YAML, or "YAML Ain't Markup Language", is a popular data serialization language that provides an easy and human-readable format for storing and sharing data. It is often used in configuration files, making it an essential tool for developers and system administrators. In this blog post, we'll explore the basics of working with YAML in Javascript and why you should consider using it in your projects.

## How To
First, we'll need to install the "js-yaml" library to parse and stringify YAML data in our Javascript code. We can use a package manager like npm to do this:

```Javascript
npm install js-yaml
```

Once the library is installed, we can import it into our code:

```Javascript
const yaml = require('js-yaml');
```

Next, let's create a YAML string with some sample data:

```Javascript
const myYAML = `
  name: John
  age: 25
  occupation: Developer
  hobbies:
    - Coding
    - Hiking
    - Cooking
`;
```

To parse this string and convert it into a Javascript object, we can use the "safeLoad" method:

```Javascript
const myObject = yaml.safeLoad(myYAML);
console.log(myObject);
```

The output will be:

```Javascript
{ 
  name: 'John',
  age: 25,
  occupation: 'Developer',
  hobbies: [ 'Coding', 'Hiking', 'Cooking' ] 
}
```

We can also go the other way and convert a Javascript object into a YAML string using the "safeDump" method:

```Javascript
const myObject = { name: 'Sarah', age: 30, occupation: 'Designer' };
const myYAML = yaml.safeDump(myObject);
console.log(myYAML);
```

The output will be:

```Javascript
"name: Sarah\nage: 30\noccupation: Designer"
```

## Deep Dive
YAML supports various data types such as strings, numbers, booleans, arrays, and objects. It also allows for comments and maintains indentation to represent nested data. Furthermore, it has a simple and intuitive syntax that makes it easy to read and write, even for non-technical users.

One advantage of using YAML over other data serialization formats like JSON is that it allows for multi-line strings, making it easier to write long strings without having to escape special characters. Additionally, YAML provides a convenient way to represent tabular data with its support for key-value pairs.

One thing to keep in mind when working with YAML in Javascript is that the library only supports the YAML 1.2 specification. This means that some features such as anchors and tags from the YAML 1.1 specification may not work as expected. It's essential to be familiar with the YAML specification to avoid any confusion.

## See Also
- [Official YAML Website](https://yaml.org/)
- [js-yaml Documentation](https://www.npmjs.com/package/js-yaml)
- [YAML Tutorial](https://www.tutorialspoint.com/yaml/index.htm)