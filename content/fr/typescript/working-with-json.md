---
title:                "Travailler avec JSON"
date:                  2024-02-03T19:24:28.421063-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Travailler avec le JSON (JavaScript Object Notation) implique de parser les données JSON vers et depuis un format utilisable en TypeScript. Les programmeurs font cela pour manipuler, stocker ou transmettre des données structurées facilement, car le JSON est léger, basé sur du texte et facilement lisible aussi bien par les humains que les machines.

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
