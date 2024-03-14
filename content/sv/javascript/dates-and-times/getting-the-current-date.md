---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:51.312064-07:00
description: "Att h\xE4mta det aktuella datumet i JavaScript \xE4r en grundl\xE4ggande\
  \ uppgift som inneb\xE4r att h\xE4mta och m\xF6jligen manipulera dagens datum och\
  \ tid. Programmerare\u2026"
lastmod: '2024-03-13T22:44:38.303747-06:00'
model: gpt-4-0125-preview
summary: "Att h\xE4mta det aktuella datumet i JavaScript \xE4r en grundl\xE4ggande\
  \ uppgift som inneb\xE4r att h\xE4mta och m\xF6jligen manipulera dagens datum och\
  \ tid. Programmerare\u2026"
title: "F\xE5 det aktuella datumet"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta det aktuella datumet i JavaScript är en grundläggande uppgift som innebär att hämta och möjligen manipulera dagens datum och tid. Programmerare utför detta för att visa datum på webbplatser, i applikationer, för att spåra användarinteraktioner eller för att hantera tidkänslig data.

## Hur:
I ren JavaScript använder man `Date`-objektet för att arbeta med datum och tider. Så här kan du få det aktuella datumet och tiden:

```javascript
const currentDate = new Date();
console.log(currentDate); // Exempelutskrift: Fre Apr 14 2023 12:34:56 GMT+0100 (Brittisk sommartid)
```

För att bara visa datumet i ett mer användarvänligt format kan du använda metoder som `toLocaleDateString()`:

```javascript
console.log(currentDate.toLocaleDateString()); // Exempelutskrift: 2023-04-14
```

För mer kontroll över formatet är tredjepartsbibliotek som *Moment.js* eller *date-fns* mycket populära, även om det är bra att vara medveten om att Moment.js nu anses vara ett äldre projekt i underhållsläge.

Med *Moment.js*:

```javascript
const moment = require('moment'); // antar Node.js eller använder en modulbuntare
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // Exempelutskrift: 2023-04-14
```

Med *date-fns*, som betonar modularisering vilket tillåter dig att bara importera det du behöver:

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // Exempelutskrift: 2023-04-14
```

Varje angreppssätt erbjuder olika nivåer av bekvämlighet och flexibilitet för att arbeta med datum i JavaScript, från det inbyggda `Date`-objektet till mer sofistikerade formaterings- och manipuleringsmöjligheter som finns tillgängliga genom bibliotek.
