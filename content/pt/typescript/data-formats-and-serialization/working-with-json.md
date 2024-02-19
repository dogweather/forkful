---
aliases:
- /pt/typescript/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:30.577599-07:00
description: "Trabalhar com JSON (JavaScript Object Notation) envolve analisar dados\
  \ JSON para e de um formato utiliz\xE1vel em TypeScript. Programadores fazem isso\
  \ para\u2026"
lastmod: 2024-02-18 23:08:57.906965
model: gpt-4-0125-preview
summary: "Trabalhar com JSON (JavaScript Object Notation) envolve analisar dados JSON\
  \ para e de um formato utiliz\xE1vel em TypeScript. Programadores fazem isso para\u2026"
title: Trabalhando com JSON
---

{{< edit_this_page >}}

## O que & Por quê?

Trabalhar com JSON (JavaScript Object Notation) envolve analisar dados JSON para e de um formato utilizável em TypeScript. Programadores fazem isso para manipular, armazenar ou transmitir dados estruturados facilmente, já que o JSON é leve, baseado em texto e facilmente legível tanto por humanos quanto por máquinas.

## Como fazer:

### Analisando JSON para um Objeto TypeScript
Para converter uma string JSON em um objeto TypeScript, você usa o método `JSON.parse()`. Isso é útil ao receber dados JSON de um servidor web ou ao ler um arquivo JSON.

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // Saída: John Doe
```

### Convertendo um Objeto TypeScript para String JSON
Para converter um objeto TypeScript em uma string JSON, você usa o método `JSON.stringify()`. Isso é particularmente útil quando você precisa enviar dados para um servidor web.

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // Saída: {"name":"Jane Doe","age":25}
```

### Trabalhando com Interfaces
Você pode definir interfaces TypeScript para trabalhar de forma integrada com dados JSON, garantindo a estrutura dos seus objetos.

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // Saída: 28
```

### Utilizando bibliotecas de terceiros populares
Para cenários mais complexos, como validação de esquemas ou transformação, você pode recorrer a bibliotecas como `class-transformer` ou `AJV` (Another JSON Schema Validator).

#### class-transformer
Esta biblioteca pode transformar objetos simples em instâncias de classes e vice-versa, o que é útil para verificação de tipos e manipulação.

```typescript
import "reflect-metadata";
import { plainToClass } from "class-transformer";
import { Person } from "./person";

const jsonStr = '{"name": "Mia", "age": 22}';
const person = plainToClass(Person, JSON.parse(jsonStr));

console.log(person instanceof Person); // Saída: true
console.log(person.name); // Saída: Mia
```

#### AJV
AJV é uma biblioteca que permite a validação rápida de esquemas JSON. Isso significa que você pode validar objetos JSON contra esquemas predefinidos.

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

console.log(valid); // Saída: true
if (!valid) console.log(validate.errors);
```

Com estas ferramentas e técnicas, você pode manipular dados JSON em suas aplicações TypeScript de forma eficiente, garantindo a integridade dos dados e aproveitando o poderoso sistema de tipos do TypeScript.
