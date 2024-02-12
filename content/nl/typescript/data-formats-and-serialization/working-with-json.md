---
title:                "Werken met JSON"
aliases:
- nl/typescript/working-with-json.md
date:                  2024-01-28T22:10:37.668811-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/typescript/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

JSON (JavaScript Object Notation) is een lichtgewicht dataformaat voor het opslaan en transporteren van gegevens. Programmeurs gebruiken het omdat het leesbaar, gemakkelijk te parseren is en veel gebruikt wordt in web-API's en configuraties.

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
