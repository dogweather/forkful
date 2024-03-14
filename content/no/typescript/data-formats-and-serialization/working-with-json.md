---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:58.341512-07:00
description: "\xC5 jobbe med JSON (JavaScript Object Notation) inneb\xE6rer parsing\
  \ av JSON-data til og fra et brukbart format i TypeScript. Programmerere gj\xF8\
  r dette for\u2026"
lastmod: '2024-03-13T22:44:40.553178-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med JSON (JavaScript Object Notation) inneb\xE6rer parsing av\
  \ JSON-data til og fra et brukbart format i TypeScript. Programmerere gj\xF8r dette\
  \ for\u2026"
title: Arbeider med JSON
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å jobbe med JSON (JavaScript Object Notation) innebærer parsing av JSON-data til og fra et brukbart format i TypeScript. Programmerere gjør dette for enkelt å manipulere, lagre eller overføre strukturerte data, ettersom JSON er lett, tekstbasert og lett leselig for både mennesker og maskiner.

## Hvordan:

### Parse JSON til et TypeScript-objekt
For å konvertere en JSON-streng til et TypeScript-objekt, bruker du `JSON.parse()`-metoden. Dette er nyttig når du mottar JSON-data fra en webserver eller leser en JSON-fil.

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // Utdata: John Doe
```

### Gjøre om et TypeScript-objekt til en JSON-streng
For å konvertere et TypeScript-objekt til en JSON-streng, bruker du `JSON.stringify()`-metoden. Dette er spesielt nyttig når du trenger å sende data til en webserver.

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // Utdata: {"name":"Jane Doe","age":25}
```

### Arbeide med Grensesnitt
Du kan definere TypeScript-grensesnitt for å arbeide sømløst med JSON-data ved å sikre strukturen på objektene dine.

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // Utdata: 28
```

### Bruke populære tredjepartsbiblioteker
For mer komplekse scenarioer, som skjemavalidering eller transformasjon, kan du ty til biblioteker som `class-transformer` eller `AJV` (Another JSON Schema Validator).

#### class-transformer
Dette biblioteket kan transformere vanlige objekter til klasseinstanser og motsatt, noe som er nyttig for typesjekking og manipulasjon.

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
AJV er et bibliotek som tillater rask JSON-skjemavalidering. Dette betyr at du kan validere JSON-objekter mot forhåndsdefinerte skjemaer.

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

Med disse verktøyene og teknikkene kan du effektivt håndtere JSON-data i dine TypeScript-applikasjoner, sikre dataintegritet og utnytte TypeScript sitt kraftfulle typesystem.
