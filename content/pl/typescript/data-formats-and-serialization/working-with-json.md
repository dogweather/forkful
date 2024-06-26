---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:40.722998-07:00
description: "Jak: Aby przekonwertowa\u0107 ci\u0105g JSON na obiekt TypeScript, u\u017C\
  ywa si\u0119 metody `JSON.parse()`. Jest to przydatne przy odbieraniu danych JSON\
  \ z serwera\u2026"
lastmod: '2024-03-13T22:44:35.161951-06:00'
model: gpt-4-0125-preview
summary: "Aby przekonwertowa\u0107 ci\u0105g JSON na obiekt TypeScript, u\u017Cywa\
  \ si\u0119 metody `JSON.parse()`."
title: Praca z JSON
weight: 38
---

## Jak:


### Parsowanie JSON-a na obiekt TypeScript
Aby przekonwertować ciąg JSON na obiekt TypeScript, używa się metody `JSON.parse()`. Jest to przydatne przy odbieraniu danych JSON z serwera sieciowego lub czytaniu pliku JSON.

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // Wynik: John Doe
```

### Zmiana obiektu TypeScript na ciąg JSON
Aby przekonwertować obiekt TypeScript na ciąg JSON, używa się metody `JSON.stringify()`. Jest to szczególnie przydatne, gdy potrzebujesz wysłać dane do serwera sieciowego.

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // Wynik: {"name":"Jane Doe","age":25}
```

### Praca z interfejsami
Można zdefiniować interfejsy TypeScript, aby pracować bezproblemowo z danymi JSON, zapewniając strukturę twoich obiektów.

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // Wynik: 28
```

### Korzystanie z popularnych bibliotek firm trzecich
W bardziej złożonych scenariuszach, takich jak walidacja schematu lub transformacja, można sięgnąć po biblioteki takie jak `class-transformer` lub `AJV` (Another JSON Schema Validator).

#### class-transformer
Ta biblioteka umożliwia transformowanie zwykłych obiektów na instancje klas i odwrotnie, co jest przydatne dla sprawdzania typów i manipulacji.

```typescript
import "reflect-metadata";
import { plainToClass } from "class-transformer";
import { Person } from "./person";

const jsonStr = '{"name": "Mia", "age": 22}';
const person = plainToClass(Person, JSON.parse(jsonStr));

console.log(person instanceof Person); // Wynik: true
console.log(person.name); // Wynik: Mia
```

#### AJV
AJV to biblioteka, która umożliwia szybką walidację schematu JSON. Oznacza to, że możesz sprawdzać obiekty JSON względem predefiniowanych schematów.

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

console.log(valid); // Wynik: true
if (!valid) console.log(validate.errors);
```

Z tymi narzędziami i technikami możesz efektywnie radzić sobie z danymi JSON w swoich aplikacjach TypeScript, zapewniając integralność danych i wykorzystując potężny system typów TypeScript.
