---
title:                "Working with JSON"
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON, or JavaScript Object Notation, is a lightweight data format for storing and transporting data. Programmers use it because it's easy to read/write for humans and machines can parse and generate it quickly.

## How to:
Parsing JSON in JavaScript:
```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const user = JSON.parse(jsonString);
console.log(user.name); // Output: John
```

Stringifying a JavaScript object to JSON:
```javascript
const user = { name: 'John', age: 30, city: 'New York' };
const jsonString = JSON.stringify(user);
console.log(jsonString); // Output: '{"name":"John","age":30,"city":"New York"}'
```

## Deep Dive
JSON was derived from JavaScript but is now a language-independent format. Many alternatives like XML exist, but JSON's minimal syntax has gained its popularity for API payloads. Technically, JSON is a subset of the JavaScript object literal notation with some differences, such as requiring keys to be wrapped in double quotes.

## See Also
- MDN JSON: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON
- JSON Formatter & Validator: https://jsonlint.com/
- JSON vs. XML: https://www.w3schools.com/js/js_json_xml.asp
