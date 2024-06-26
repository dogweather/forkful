---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:30.985712-07:00
description: "Come fare: Per convertire una stringa JSON in un oggetto TypeScript,\
  \ si utilizza il metodo `JSON.parse()`. Questo \xE8 utile quando si ricevono dati\
  \ JSON da\u2026"
lastmod: '2024-03-13T22:44:43.197485-06:00'
model: gpt-4-0125-preview
summary: Per convertire una stringa JSON in un oggetto TypeScript, si utilizza il
  metodo `JSON.parse()`.
title: Lavorare con JSON
weight: 38
---

## Come fare:


### Analizzare JSON in un Oggetto TypeScript
Per convertire una stringa JSON in un oggetto TypeScript, si utilizza il metodo `JSON.parse()`. Questo è utile quando si ricevono dati JSON da un server web o si legge un file JSON.

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // Output: John Doe
```

### Convertire un Oggetto TypeScript in una Stringa JSON
Per convertire un oggetto TypeScript in una stringa JSON, si utilizza il metodo `JSON.stringify()`. Questo è particolarmente utile quando è necessario inviare dati a un server web.

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // Output: {"name":"Jane Doe","age":25}
```

### Lavorare con le Interfacce
È possibile definire interfacce TypeScript per lavorare in modo efficiente con dati JSON, garantendo la struttura dei propri oggetti.

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // Output: 28
```

### Utilizzare librerie di terze parti popolari
Per scenari più complessi, come la validazione dello schema o la trasformazione, potresti ricorrere a librerie come `class-transformer` o `AJV` (Un Altro Validatore di Schema JSON).

#### class-transformer
Questa libreria può trasformare oggetti semplici in istanze di classi e viceversa, il che è utile per il controllo e la manipolazione dei tipi.

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
AJV è una libreria che consente una rapida validazione dello schema JSON. Questo significa che puoi validare oggetti JSON contro schemi predefiniti.

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

Con questi strumenti e tecniche, puoi gestire efficientemente i dati JSON nelle tue applicazioni TypeScript, garantendo l'integrità dei dati e sfruttando il potente sistema di tipi di TypeScript.
