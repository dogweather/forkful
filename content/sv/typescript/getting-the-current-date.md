---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:16:53.808258-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta det aktuella datumet innebär att få tag på det nuvarande datumet och tiden där programmet körs. Programmerare gör detta för att logga händelser, sätta tidsstämplar, eller hantera schemaläggning och påminnelser.

## Hur gör man:
Lite TypeScript-kod för att komma igång.

```typescript
const nu = new Date();
console.log(nu.toString()); // "Tue Mar 21 2023 14:25:35 GMT+0100 (Central European Standard Time)"
console.log(nu.toISOString()); // "2023-03-21T13:25:35.000Z"
```

Kör den här koden, och du får det aktuella datumet och tiden utskrivet. `toString()` ger dig en läsbar string, medan `toISOString()` ger en UTC-tid i ISO 8601-format.

## Djupdykning
`Date`-objektet i JavaScript, och därmed i TypeScript, har sitt ursprung från Java. Det skapades för många år sedan och har blivit en standard för datum- och tidshantering i webbutveckling. Alternativ till den inbyggda `Date` inkluderar bibliotek som Moment.js och Date-fns, vilka erbjuder mer kraftfulla verktyg för datumanipulation. Den största skillnaden mellan dessa bibliotek och inbyggda datumfunktioner är deras flexibilitet och ytterligare funktionalitet. När det gäller implementeringsdetaljer kan arbetet med tidszoner vara klurigt; JavaScript hanterar detta genom att `Date`-objektet internt använder UTC och kan konvertera till och från lokal tid.

## Se även
- MDN Web Docs om Date-objektet: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Date-fns dokumentation: [https://date-fns.org/](https://date-fns.org/)
- Moment.js hemsida: [https://momentjs.com/](https://momentjs.com/)
