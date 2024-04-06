---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:37.668811-07:00
description: 'Hoe te: **JSON Parseren:**.'
lastmod: '2024-04-05T21:53:50.594544-06:00'
model: gpt-4-0125-preview
summary: ''
title: Werken met JSON
weight: 38
---

## Hoe te:
**JSON Parseren:**

```TypeScript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
let user = JSON.parse(jsonString);
console.log(user.name); // John
```

**JavaScript-objecten omzetten naar een string:**

```TypeScript
const userObject = { name: 'Jane', age: 25, city: 'Los Angeles' };
let jsonOutput = JSON.stringify(userObject);
console.log(jsonOutput); // {"name":"Jane","age":25,"city":"Los Angeles"}
```

**Typeverklaringen:**

```TypeScript
type User = {
  name: string;
  age: number;
  city: string;
};

const userJson = '{"name":"Jack", "age":28, "city":"Chicago"}';
let user: User = JSON.parse(userJson);
console.log(user.city); // Chicago
```

## Diepgaande duik
JSON is begonnen vanuit JavaScript maar is nu taalonafhankelijk; het is de go-to geworden voor gegevensuitwisseling, XML vervangend vanwege zijn eenvoud. Hoewel JSON van nature geen typen afdwingt (wat TypeScript allemaal doet), laat TypeScript je typen definiëren om ervoor te zorgen dat je JSON-structuur is wat je verwacht. En hoewel JSON koning is voor API's, geven sommigen voor configuratiebestanden de voorkeur aan YAML, dat meer leesbaar is voor mensen. Achter de schermen, wanneer `JSON.parse()` of `JSON.stringify()` wordt aangeroepen in TypeScript, roept het eigenlijk de JSON-functies van de JavaScript-engine aan; de hoofdrol van TypeScript is om deze operaties te versterken met typeveiligheid.

## Zie ook
- [JSON.org](https://www.json.org/json-en.html): Officiële JSON-documentatie.
- [MDN - Werken met JSON](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON): Het goede oude MDN biedt een algemene achtergrond en gebruikscases.
