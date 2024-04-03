---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:22.573258-07:00
description: "Associativa arrayer, eller objekt i TypeScript, l\xE5ter dig anv\xE4\
  nda str\xE4ngar (eller nycklar) f\xF6r att komma \xE5t v\xE4rdepar. Programmerare\
  \ anv\xE4nder dem f\xF6r mer\u2026"
lastmod: '2024-03-13T22:44:37.649138-06:00'
model: gpt-4-0125-preview
summary: "Associativa arrayer, eller objekt i TypeScript, l\xE5ter dig anv\xE4nda\
  \ str\xE4ngar (eller nycklar) f\xF6r att komma \xE5t v\xE4rdepar."
title: "Att anv\xE4nda associativa arrayer"
weight: 15
---

## Hur man gör:
Att skapa och använda associativa arrayer i TypeScript är enkelt. Här är en grundläggande genomgång:

```TypeScript
// Deklarera en associativ array
let user: { [key: string]: string } = {};

// Lägga till data
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

Utdata:

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

Det är också lätt att iterera över nyckel-värdepar:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

Utdata:

```TypeScript
name: Jane Doe
email: jane@example.com
```

Och om du hanterar en blandning av datatyper kommer TypeScripts typsystem väl till pass:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

Utdata:

```TypeScript
{ name: 'John Doe', age: 30 }
```

## Fördjupning
I TypeScript är det vi refererar till som associativa arrayer i grund och botten objekt. Historiskt sett, i språk som PHP, är associativa arrayer en grundläggande typ, men JavaScript (och därmed TypeScript) använder objekt för detta ändamål. Detta tillvägagångssätt är både en styrka och en begränsning. Objekt ger en mycket dynamisk struktur för att koppla strängar till värden, men de är inte avsedda att användas som 'arrayer' i traditionell mening. Till exempel kan du inte direkt använda arraymetoder som `push` eller `pop` på dessa objekt.

För fall där du behöver ordnade samlingar av nyckel-värdepar med array-liknande operationer, erbjuder TypeScript (och modern JavaScript) `Map`-objektet:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

Även om TypeScripts typsystem och ES6-funktioner som `Map` erbjuder kraftfulla alternativ, är förståelsen av hur man använder objekt som associativa arrayer användbar för scenarier där objektliteraler är mer effektiva eller när man arbetar med JSON-datastrukturer. Det handlar allt om att välja rätt verktyg för jobbet.
