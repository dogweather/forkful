---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:52.346678-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON (JavaScript Object\
  \ Notation) \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\
  \u0437 \u0434\u0430\u043D\u0438\u0445 JSON \u0434\u043E \u0456 \u0437 \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u043D\u043E\u0433\
  \u043E \u0444\u043E\u0440\u043C\u0430\u0442\u0443 \u0432 TypeScript. \u041F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\
  \u0442\u044C \u0446\u0435, \u0449\u043E\u0431 \u043B\u0435\u0433\u043A\u043E\u2026"
lastmod: '2024-03-11T00:14:22.739982-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON (JavaScript Object Notation)\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0430\u043D\u0430\u043B\u0456\u0437\
  \ \u0434\u0430\u043D\u0438\u0445 JSON \u0434\u043E \u0456 \u0437 \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u043D\u043E\u0433\u043E\
  \ \u0444\u043E\u0440\u043C\u0430\u0442\u0443 \u0432 TypeScript. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\
  \u044C \u0446\u0435, \u0449\u043E\u0431 \u043B\u0435\u0433\u043A\u043E\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
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
