---
title:                "Робота з JSON"
aliases: - /uk/typescript/working-with-json.md
date:                  2024-02-03T19:24:52.346678-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Робота з JSON (JavaScript Object Notation) включає аналіз даних JSON до і з використовуваного формату в TypeScript. Програмісти роблять це, щоб легко маніпулювати, зберігати або передавати структуровані дані, оскільки JSON є легковісним, текстовим і легко читаним як для людей, так і для машин.

## Як:

### Парсинг JSON у об'єкт TypeScript
Щоб перетворити рядок JSON на об'єкт TypeScript, використовуйте метод `JSON.parse()`. Це корисно, коли отримуєте дані JSON від веб-сервера або читаєте файл JSON.

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // Вивід: John Doe
```

### Перетворення об'єкта TypeScript на рядок JSON
Щоб перетворити об'єкт TypeScript на рядок JSON, використовуйте метод `JSON.stringify()`. Це особливо корисно, коли вам потрібно надіслати дані на веб-сервер.

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // Вивід: {"name":"Jane Doe","age":25}
```

### Робота з інтерфейсами
Ви можете визначити інтерфейси TypeScript для роботи з даними JSON безперебійно, забезпечуючи структуру ваших об'єктів.

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // Вивід: 28
```

### Використання популярних сторонніх бібліотек
Для більш складних сценаріїв, таких як перевірка схеми або трансформація, ви можете вдатися до бібліотек, таких як `class-transformer` або `AJV` (Another JSON Schema Validator).

#### class-transformer
Ця бібліотека може перетворювати звичайні об'єкти на екземпляри класів і навпаки, що корисно для перевірки типів і маніпуляцій.

```typescript
import "reflect-metadata";
import { plainToClass } from "class-transformer";
import { Person } from "./person";

const jsonStr = '{"name": "Mia", "age": 22}';
const person = plainToClass(Person, JSON.parse(jsonStr));

console.log(person instanceof Person); // Вивід: true
console.log(person.name); // Вивід: Mia
```

#### AJV
AJV - це бібліотека, яка дозволяє швидко перевіряти схеми JSON. Це означає, що ви можете перевіряти об'єкти JSON на відповідність попередньо визначеним схемам.

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

console.log(valid); // Вивід: true
if (!valid) console.log(validate.errors);
```

З цими інструментами та техніками ви можете ефективно обробляти дані JSON у ваших додатках TypeScript, забезпечуючи цілісність даних і використовуючи потужну систему типів TypeScript.
