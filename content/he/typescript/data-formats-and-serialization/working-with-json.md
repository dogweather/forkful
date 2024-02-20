---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:56.617643-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON (JavaScript Object\
  \ Notation) \u05DB\u05D5\u05DC\u05DC\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 \u05E0\
  \u05EA\u05D5\u05E0\u05D9 JSON \u05DC\u05EA\u05D5\u05DA \u05D5\u05DE\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E9\u05D9\u05DE\u05D5\u05E9\u05D9 \u05D1TypeScript.\
  \ \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05E9\u05DC\u05D5\u05D8, \u05DC\
  \u05D0\u05D7\u05E1\u05DF \u05D0\u05D5 \u05DC\u05E9\u05D3\u05E8\u2026"
lastmod: 2024-02-19 22:04:58.161403
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON (JavaScript Object Notation)\
  \ \u05DB\u05D5\u05DC\u05DC\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 \u05E0\u05EA\u05D5\
  \u05E0\u05D9 JSON \u05DC\u05EA\u05D5\u05DA \u05D5\u05DE\u05EA\u05D5\u05DA \u05E4\
  \u05D5\u05E8\u05DE\u05D8 \u05E9\u05D9\u05DE\u05D5\u05E9\u05D9 \u05D1TypeScript.\
  \ \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05E9\u05DC\u05D5\u05D8, \u05DC\
  \u05D0\u05D7\u05E1\u05DF \u05D0\u05D5 \u05DC\u05E9\u05D3\u05E8\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם JSON (JavaScript Object Notation) כוללת ניתוח נתוני JSON לתוך ומתוך פורמט שימושי בTypeScript. מתכנתים עושים זאת על מנת לשלוט, לאחסן או לשדר נתונים מובנים בקלות, מכיוון ש-JSON קל משקל, מבוסס טקסט וקריא בקלות על ידי בני אדם ומכונות כאחד.

## איך לעשות:

### ניתוח JSON לאובייקט של TypeScript
להמיר מחרוזת JSON לאובייקט של TypeScript, אתה משתמש במתודה `JSON.parse()`. זה שימושי כאשר מקבלים נתוני JSON משרת וב \ או קוראים קובץ JSON.

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // פלט: John Doe
```

### המרת אובייקט של TypeScript למחרוזת JSON
להמיר אובייקט של TypeScript למחרוזת JSON, אתה משתמש במתודה `JSON.stringify()`. זה בעיקר שימושי כאשר אתה צריך לשלוח נתונים לשרת וב.

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // פלט: {"name":"Jane Doe","age":25}
```

### עבודה עם ממשקים
אתה יכול להגדיר ממשקים של TypeScript על מנת לעבוד בצורה חלקה עם נתוני JSON על ידי הבטחה של מבנה האובייקטים שלך.

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // פלט: 28
```

### שימוש בספריות צד שלישי פופולריות
לתרחישים מורכבים יותר, כמו אימות סכימה או המרה, תוכל להשתמש בספריות כמו `class-transformer` או `AJV` (Another JSON Schema Validator).

#### class-transformer
ספריה זו יכולה להמיר אובייקטים פשוטים לדוגמאות של מחלקות ולהפך, שימושי לבדיקת סוגים ולמניפולציה.

```typescript
import "reflect-metadata";
import { plainToClass } from "class-transformer";
import { Person } from "./person";

const jsonStr = '{"name": "Mia", "age": 22}';
const person = plainToClass(Person, JSON.parse(jsonStr));

console.log(person instanceof Person); // פלט: true
console.log(person.name); // פלט: Mia
```

#### AJV
AJV היא ספריה המאפשרת אימות סכימות JSON מהיר. זה אומר שאתה יכול לאמת אובייקטים של JSON נגד סכימות מוגדרות מראש.

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

console.log(valid); // פלט: true
if (!valid) console.log(validate.errors);
```

עם כלים וטכניקות אלו, תוכל לנהל נתוני JSON באפליקציות של TypeScript שלך ביעילות, מבטיח את שלמות הנתונים ונצל את מערכת הסוגים העוצמתית של TypeScript.
