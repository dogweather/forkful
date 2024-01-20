---
title:                "Working with json"
html_title:           "Arduino recipe: Working with json"
simple_title:         "Working with json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON (JavaScript Object Notation) is a lightweight data format for storing and transporting data. Programmers use it because it's readable, easy to parse, and readily used in web APIs and configs.

## How to:

**Parse JSON:**

```TypeScript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
let user = JSON.parse(jsonString);
console.log(user.name); // John
```

**Stringify JavaScript objects:**

```TypeScript
const userObject = { name: 'Jane', age: 25, city: 'Los Angeles' };
let jsonOutput = JSON.stringify(userObject);
console.log(jsonOutput); // {"name":"Jane","age":25,"city":"Los Angeles"}
```

**Type Declarations:**

```TypeScript
type User = {
  name: string;
  age: number;
  city: string;
};

const userJson = '{"name":"Jack", "age":28, "city":"Chicago"}';
let user: User = JSON.parse(userJson);
console.log(user.city); // Chicago
```

## Deep Dive

JSON's got its start from JavaScript but is now language-agnostic; it's become the go-to for data interchange, replacing XML due to its simplicity. Although JSON natively doesn't enforce types (which TypeScript is all about), TypeScript lets you define types to ensure your JSON structure is what you expect. And while JSON is king for APIs, for configuration files, some prefer YAML, which is more human-readable. Under the hood, when `JSON.parse()` or `JSON.stringify()` is called in TypeScript, it's actually calling the JavaScript engine's JSON functions; TypeScript's main role is to enhance these operations with type safety.

## See Also

- [JSON.org](https://www.json.org/json-en.html): Official JSON documentation.
- [MDN - Working with JSON](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON): Good old MDN provides a general background and use cases.