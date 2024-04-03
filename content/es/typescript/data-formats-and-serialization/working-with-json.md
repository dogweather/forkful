---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:29.321969-07:00
description: "Trabajar con JSON (JavaScript Object Notation) implica analizar datos\
  \ JSON para convertirlos de y hacia un formato utilizable en TypeScript. Los\u2026"
lastmod: '2024-03-13T22:44:58.824864-06:00'
model: gpt-4-0125-preview
summary: Trabajar con JSON (JavaScript Object Notation) implica analizar datos JSON
  para convertirlos de y hacia un formato utilizable en TypeScript.
title: Trabajando con JSON
weight: 38
---

## Qué y Por Qué?

Trabajar con JSON (JavaScript Object Notation) implica analizar datos JSON para convertirlos de y hacia un formato utilizable en TypeScript. Los programadores hacen esto para manipular, almacenar o transmitir datos estructurados fácilmente, dado que JSON es liviano, basado en texto y fácilmente legible tanto por humanos como por máquinas.

## Cómo:

### Analizando JSON a un Objeto TypeScript
Para convertir una cadena JSON a un objeto TypeScript, utilizas el método `JSON.parse()`. Esto es útil al recibir datos JSON de un servidor web o al leer un archivo JSON.

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // Salida: John Doe
```

### Convirtiendo un Objeto TypeScript a una Cadena JSON
Para convertir un objeto TypeScript a una cadena JSON, utilizas el método `JSON.stringify()`. Esto es particularmente útil cuando necesitas enviar datos a un servidor web.

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // Salida: {"name":"Jane Doe","age":25}
```

### Trabajando con Interfaces
Puedes definir interfaces TypeScript para trabajar de manera fluida con datos JSON asegurando la estructura de tus objetos.

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // Salida: 28
```

### Uso de bibliotecas de terceros populares
Para escenarios más complejos, como la validación de esquemas o la transformación, podrías recurrir a bibliotecas como `class-transformer` o `AJV` (Another JSON Schema Validator).

#### class-transformer
Esta biblioteca puede transformar objetos planos en instancias de clase y viceversa, lo cual es útil para la comprobación y manipulación de tipos.

```typescript
import "reflect-metadata";
import { plainToClass } from "class-transformer";
import { Person } from "./person";

const jsonStr = '{"name": "Mia", "age": 22}';
const person = plainToClass(Person, JSON.parse(jsonStr));

console.log(person instanceof Person); // Salida: true
console.log(person.name); // Salida: Mia
```

#### AJV
AJV es una biblioteca que permite la validación rápida de esquemas JSON. Esto significa que puedes validar objetos JSON contra esquemas predefinidos.

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

console.log(valid); // Salida: true
if (!valid) console.log(validate.errors);
```

Con estas herramientas y técnicas, puedes manejar eficientemente los datos JSON en tus aplicaciones TypeScript, asegurando la integridad de los datos y aprovechando el poderoso sistema de tipos de TypeScript.
