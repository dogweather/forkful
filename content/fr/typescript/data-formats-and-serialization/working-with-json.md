---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:28.421063-07:00
description: "Comment faire : Pour convertir une cha\xEEne JSON en un objet TypeScript,\
  \ vous utilisez la m\xE9thode `JSON.parse()`. Cela est utile lors de la r\xE9ception\
  \ de\u2026"
lastmod: '2024-03-13T22:44:57.460017-06:00'
model: gpt-4-0125-preview
summary: "Pour convertir une cha\xEEne JSON en un objet TypeScript, vous utilisez\
  \ la m\xE9thode `JSON.parse()`."
title: Travailler avec JSON
weight: 38
---

## Comment faire :


### Parser du JSON en un Objet TypeScript
Pour convertir une chaîne JSON en un objet TypeScript, vous utilisez la méthode `JSON.parse()`. Cela est utile lors de la réception de données JSON d'un serveur web ou lors de la lecture d'un fichier JSON.

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // Sortie : John Doe
```

### Transformer un Objet TypeScript en Chaîne JSON
Pour convertir un objet TypeScript en chaîne JSON, vous utilisez la méthode `JSON.stringify()`. Cela est particulièrement utile lorsque vous avez besoin d'envoyer des données à un serveur web.

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // Sortie : {"name":"Jane Doe","age":25}
```

### Travailler avec les Interfaces
Vous pouvez définir des interfaces TypeScript pour travailler de manière transparente avec les données JSON en assurant la structure de vos objets.

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // Sortie : 28
```

### Utiliser des bibliothèques tierces populaires
Pour des scénarios plus complexes, comme la validation de schéma ou la transformation, vous pourriez avoir recours à des bibliothèques comme `class-transformer` ou `AJV` (Another JSON Schema Validator).

#### class-transformer
Cette bibliothèque peut transformer des objets simples en instances de classe et vice-versa, ce qui est utile pour la vérification et la manipulation des types.

```typescript
import "reflect-metadata";
import { plainToClass } from "class-transformer";
import { Person } from "./person";

const jsonStr = '{"name": "Mia", "age": 22}';
const person = plainToClass(Person, JSON.parse(jsonStr));

console.log(person instanceof Person); // Sortie : true
console.log(person.name); // Sortie : Mia
```

#### AJV
AJV est une bibliothèque qui permet une validation rapide du schéma JSON. Cela signifie que vous pouvez valider des objets JSON contre des schémas préétablis.

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

console.log(valid); // Sortie : true
if (!valid) console.log(validate.errors);
```

Avec ces outils et techniques, vous pouvez gérer de manière efficace les données JSON dans vos applications TypeScript, en assurant l'intégrité des données et en tirant parti du système de type puissant de TypeScript.
