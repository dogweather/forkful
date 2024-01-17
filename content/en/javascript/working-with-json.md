---
title:                "Working with json"
html_title:           "Javascript recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) is a lightweight data interchange format that is commonly used by programmers to transfer data between applications. It is a popular choice due to its simple structure, human-readable format, and platform independence. JSON has become a standard for data exchange in web development, making it an essential tool for any programmer working with web applications.

## How to:
To use JSON in your Javascript code, you can start by creating a simple object with properties and values. For example:
```javascript
const person = {
  name: "John",
  age: 30,
  occupation: "Developer"
}
```
To convert this object into a JSON string, you can use the `JSON.stringify()` method:
```javascript
const personJSON = JSON.stringify(person);
```
The output will be a JSON string with the same properties and values as the original object:
```javascript
{"name":"John","age":30,"occupation":"Developer"}
```
To convert a JSON string back into an object, you can use the `JSON.parse()` method:
```javascript
const newPerson = JSON.parse(personJSON);
```
This will return an object with the same properties and values as the original object:
```javascript
{name: "John", age: 30, occupation: "Developer"}
```

## Deep Dive:
- JSON was first specified in 2002 by Douglas Crockford, making it a relatively new data format. Its popularity grew with the rise of Ajax and has become the preferred way to exchange data between client and server.
- There are many alternatives to JSON, such as XML and YAML. However, JSON's lightweight structure and easier interchangeability have made it the go-to choice for data exchange.
- In Javascript, JSON is built-in and does not require any additional libraries or dependencies. This makes it easy to use and accessible for all levels of programmers.

## See Also:
- [MDN web docs - JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [W3Schools - JSON Tutorial](https://www.w3schools.com/js/js_json_intro.asp)
- [JSON official website](https://www.json.org/json-en.html)