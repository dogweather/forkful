---
date: 2024-01-20 17:37:37.974193-07:00
description: "Hur man g\xF6r: Konvertering av datum till str\xE4ngar har varit en\
  \ del av programmeringen sedan b\xF6rjan. Alternativ inkluderar anv\xE4ndning av\
  \ bibliotek som\u2026"
lastmod: '2024-04-05T22:50:51.953734-06:00'
model: gpt-4-1106-preview
summary: "Konvertering av datum till str\xE4ngar har varit en del av programmeringen\
  \ sedan b\xF6rjan."
title: "Omvandla ett datum till en str\xE4ng"
weight: 28
---

## Hur man gör:
```TypeScript
const formatDate = (date: Date): string => {
  return date.toISOString();
};

const now = new Date();
const dateString = formatDate(now);
console.log(dateString); // Exempel-utskrift: "2023-04-05T14:20:30.45Z"
```

Ett annat sätt:
```TypeScript
const options: Intl.DateTimeFormatOptions = {
  year: 'numeric', month: 'long', day: 'numeric',
  hour: '2-digit', minute: '2-digit', second: '2-digit',
  timeZoneName: 'short'
};
const localDateString = new Date().toLocaleDateString('sv-SE', options);
console.log(localDateString); // Exempel-utskrift: "5 april 2023 kl. 16:20:30 CEST"
```

## Djupdykning
Konvertering av datum till strängar har varit en del av programmeringen sedan början. Alternativ inkluderar användning av bibliotek som Moment.js, men det är numer fasat till fördel för moderna API:er som `Date`-objektet och `Intl.DateTimeFormat`.

För server- och applikationsloggar är det standard att använda ISO-strängar (`.toISOString()`) för att garantera formatkonsistens över olika system. Det lokala formatet (`.toLocaleDateString()`) tillåter istället anpassning utefter språk och regioninställningar.

Detaljerna i implementeringen handlar om att hantera tidszoner och format. TypeScript bygger på JavaScript och använder dess datumfunktioner. Typsäkerhet i TypeScript förbättrar dock hanteringen genom att klargöra vad som överförs eller returneras av olika funktioner.

## Se också
- MDN Web Docs, "Date" – https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- MDN Web Docs, "Intl.DateTimeFormat" – https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat
- "You Don't Need Moment.js" – https://you-dont-need-momentjs.com/
