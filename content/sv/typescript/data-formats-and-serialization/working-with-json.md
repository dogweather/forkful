---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:33.727342-07:00
description: "Att arbeta med JSON (JavaScript Object Notation) inneb\xE4r att tolka\
  \ JSON-data till och fr\xE5n ett anv\xE4ndbart format i TypeScript. Programmerare\
  \ g\xF6r detta f\xF6r\u2026"
lastmod: '2024-03-11T00:14:11.016327-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med JSON (JavaScript Object Notation) inneb\xE4r att tolka JSON-data\
  \ till och fr\xE5n ett anv\xE4ndbart format i TypeScript. Programmerare g\xF6r detta\
  \ f\xF6r\u2026"
title: Arbeta med JSON
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med JSON (JavaScript Object Notation) innebär att tolka JSON-data till och från ett användbart format i TypeScript. Programmerare gör detta för att enkelt manipulera, lagra eller överföra strukturerad data, eftersom JSON är lättviktigt, textbaserat och lättläst för både människor och maskiner.

## Hur man gör:

### Tolka JSON till ett TypeScript-objekt
För att konvertera en JSON-sträng till ett TypeScript-objekt använder du metoden `JSON.parse()`. Detta är användbart när du tar emot JSON-data från en webbserver eller läser en JSON-fil.

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // Utdata: John Doe
```

### Omvandla ett TypeScript-objekt till JSON
För att konvertera ett TypeScript-objekt till en JSON-sträng använder du metoden `JSON.stringify()`. Detta är särskilt användbart när du behöver skicka data till en webbserver.

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // Utdata: {"name":"Jane Doe","age":25}
```

### Arbeta med gränssnitt
Du kan definiera TypeScript-gränssnitt för att arbeta sömlöst med JSON-data genom att säkerställa strukturen på dina objekt.

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // Utdata: 28
```

### Använda populära tredjepartsbibliotek
För mer komplexa scenarion, som schemavalidering eller transformation, kan du använda bibliotek som `class-transformer` eller `AJV` (Another JSON Schema Validator).

#### class-transformer
Detta bibliotek kan omvandla vanliga objekt till klassinstanser och vice versa, vilket är användbart för typkontroll och manipulation.

```typescript
import "reflect-metadata";
import { plainToClass } from "class-transformer";
import { Person } from "./person";

const jsonStr = '{"name": "Mia", "age": 22}';
const person = plainToClass(Person, JSON.parse(jsonStr));

console.log(person instanceof Person); // Utdata: true
console.log(person.name); // Utdata: Mia
```

#### AJV
AJV är ett bibliotek som tillåter snabb JSON-schema validering. Det betyder att du kan validera JSON-objekt mot fördefinierade scheman.

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

console.log(valid); // Utdata: true
if (!valid) console.log(validate.errors);
```

Med dessa verktyg och tekniker kan du effektivt hantera JSON-data i dina TypeScript-applikationer, säkerställa dataintegritet och utnyttja TypeScript:s kraftfulla typsystem.
