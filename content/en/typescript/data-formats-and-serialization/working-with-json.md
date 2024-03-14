---
date: 2024-02-03 19:03:18.338680-07:00
description: "Working with JSON (JavaScript Object Notation) involves parsing JSON\
  \ data to and from a usable format in TypeScript. Programmers do this to manipulate,\u2026"
lastmod: '2024-03-13T22:44:59.877551-06:00'
model: gpt-4-0125-preview
summary: "Working with JSON (JavaScript Object Notation) involves parsing JSON data\
  \ to and from a usable format in TypeScript. Programmers do this to manipulate,\u2026"
title: Working with JSON
---

{{< edit_this_page >}}

## What & Why?

Working with JSON (JavaScript Object Notation) involves parsing JSON data to and from a usable format in TypeScript. Programmers do this to manipulate, store, or transmit structured data easily, as JSON is lightweight, text-based, and easily readable by humans and machines alike.

## How to:

### Parsing JSON to a TypeScript Object
To convert a JSON string to a TypeScript object, you use the `JSON.parse()` method. This is useful when receiving JSON data from a web server or reading a JSON file.

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // Output: John Doe
```

### Stringifying a TypeScript Object to JSON
To convert a TypeScript object to a JSON string, you use the `JSON.stringify()` method. This is particularly useful when you need to send data to a web server.

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // Output: {"name":"Jane Doe","age":25}
```

### Working with Interfaces
You can define TypeScript interfaces to work seamlessly with JSON data by ensuring the structure of your objects.

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // Output: 28
```

### Using popular third-party libraries
For more complex scenarios, like schema validation or transformation, you might resort to libraries like `class-transformer` or `AJV` (Another JSON Schema Validator).

#### class-transformer
This library can transform plain objects to class instances and vice versa, which is useful for type checking and manipulation.

```typescript
import "reflect-metadata";
import { plainToClass } from "class-transformer";
import { Person } from "./person";

const jsonStr = '{"name": "Mia", "age": 22}';
const person = plainToClass(Person, JSON.parse(jsonStr));

console.log(person instanceof Person); // Output: true
console.log(person.name); // Output: Mia
```

#### AJV
AJV is a library that allows for fast JSON schema validation. This means you can validate JSON objects against predefined schemas.

```typescript
import Ajv from "ajv";

const ajv = new Ajv();

const schema = {
  type: "object",
  properties: {
    name: { type: "string" },
    age: { type: "number" },
  },
  required: ["name", "age"],
  additionalProperties: false,
};

const validate = ajv.compile(schema);
const valid = validate({ name: "Tom", age: 24 });

console.log(valid); // Output: true
if (!valid) console.log(validate.errors);
```

With these tools and techniques, you can efficiently handle JSON data in your TypeScript applications, ensuring data integrity and leveraging TypeScript's powerful type system.
