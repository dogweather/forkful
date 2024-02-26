---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:19.255255-07:00
description: "De huidige datum in je code krijgen betekent het vastleggen van het\
  \ huidige moment tot op de dag. Programmeurs doen dit om gebeurtenissen te\u2026"
lastmod: '2024-02-25T18:49:47.907194-07:00'
model: gpt-4-0125-preview
summary: "De huidige datum in je code krijgen betekent het vastleggen van het huidige\
  \ moment tot op de dag. Programmeurs doen dit om gebeurtenissen te\u2026"
title: Het huidige datum ophalen
---

{{< edit_this_page >}}

## Wat & Waarom?
De huidige datum in je code krijgen betekent het vastleggen van het huidige moment tot op de dag. Programmeurs doen dit om gebeurtenissen te tijdstempelen, planningen te beheren, en de duur of intervallen bij te houden.

## Hoe:
Hier is hoe je de huidige datum in TypeScript vastlegt:

```typescript
// Haal de huidige datum en tijd op
const now = new Date();

// Log het naar de console
console.log(now);
```

Een voorbeelduitvoer kan er zo uitzien:

```
2023-04-01T12:34:56.789Z
```

Maar als je alleen de datum zonder de tijd wilt:

```typescript
const today = new Date().toISOString().split('T')[0];

console.log(today);
```

En dat geeft je:

```
2023-04-01
```

## Diepgaand
Het `Date` object van JavaScript is wat je gebruikt in TypeScript voor datums en tijden. Het bestaat al sinds de vroege dagen, gecreëerd als onderdeel van ECMAScript 1 in 1997. Alternatieven voor de native `Date` omvatten bibliotheken zoals `moment.js` of `date-fns`, die meer functies en betere parsing bieden.

Achter de schermen haalt `new Date()` het aantal milliseconden sinds het Unix Epoch (1 januari 1970) op. Zo houden computers tijd bij. Tijdzones kunnen lastig zijn, vooral wanneer je datums aan gebruikers over de hele wereld moet tonen. Standaard zal `new Date()` de lokale tijd van het systeem gebruiken. De `toISOString()` methode zet de datum om naar Coordinated Universal Time (UTC) en formatteert het als een ISO-string.

## Zie Ook
- MDN Web Docs over `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/
- Date-fns: https://date-fns.org/
- Tijdzoneafhandeling in JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleTimeString
