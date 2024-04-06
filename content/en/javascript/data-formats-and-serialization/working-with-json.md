---
date: 2024-02-03 19:03:07.612668-07:00
description: 'How to: To convert a JSON string into a JavaScript object, use `JSON.parse()`.'
lastmod: '2024-03-13T22:45:00.453500-06:00'
model: gpt-4-0125-preview
summary: To convert a JSON string into a JavaScript object, use `JSON.parse()`.
title: Working with JSON
weight: 38
---

## How to:


### Parsing JSON
To convert a JSON string into a JavaScript object, use `JSON.parse()`.

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // Output: John
```

### Stringifying JavaScript Objects
To convert a JavaScript object back into a JSON string, use `JSON.stringify()`.

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // Output: {"name":"Jane","age":25,"city":"London"}
```

### Working with Files in Node.js
To read a JSON file and convert it into an object in a Node.js environment, you can use the `fs` module. This example assumes you have a file named `data.json`.

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) throw err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

For writing an object to a JSON file:

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) throw err;
    console.log('Data written to file');
});
```

### Third-Party Libraries
For complex JSON operations, frameworks and libraries like `lodash` can simplify tasks, but for basic operations, native JavaScript functions are often sufficient. For large scale or performance-critical applications, you can consider libraries like `fast-json-stringify` for faster JSON stringification or `json5` for parsing and stringify using a more flexible JSON format.

Parsing with `json5`:
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // Output: John
```

These examples cover basic operations with JSON in JavaScript, perfect for beginners transitioning from other languages and looking to handle data in web applications efficiently.
