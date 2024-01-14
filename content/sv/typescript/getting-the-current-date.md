---
title:                "TypeScript: Att hämta nuvarande datum"
simple_title:         "Att hämta nuvarande datum"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna få den aktuella tiden och datumet är en viktig del av programmering. Oavsett om man behöver spåra användaraktiviteter, schemalägga processer eller bara visa den aktuella tiden till användaren, är det nödvändigt att kunna hämta nuvarande datum och tid i projektet. I denna bloggpost kommer vi att utforska hur man enkelt kan göra det med TypeScript.

## Så här gör du

För att få den aktuella tiden och datumet med TypeScript, behöver du använda Date-objektet och dess inbyggda metoder. Nedan är en kod som visar hur du kan göra det:

```TypeScript
let nu = new Date(); // Skapar ett Date-objekt som representerar nuvarande tiden
console.log(nu); // Skriver ut hela objektet (ex: Fri Jul 16 2021 12:36:46 GMT+0200)
console.log(nu.getHours()); // Hämtar timmen från objektet (ex: 12)
console.log(nu.getMinutes()); // Hämtar minuten från objektet (ex: 36)
console.log(nu.getDate()); // Hämtar datumet från objektet (ex: 16)
console.log(nu.getMonth()); // Hämtar månaden från objektet (ex: 6)
console.log(nu.getFullYear()); // Hämtar året från objektet (ex: 2021)
```

Som du kan se från kodexemplet ovan, kan du enkelt hämta olika delar av det aktuella datumet och dess tid genom att använda lämpliga metoder på Date-objektet. Du kan också göra andra manipulationer som att lägga till eller subtrahera tid från det aktuella datumet och tiden. För mer information om alla tillgängliga metoder inom Date-objektet, kan du kolla in dokumentationen på TypeScript's hemsida.

## Djupdykning

En viktig sak att notera är att Date-objektet i TypeScript används för både datum och tid. Detta betyder att om du bara vill hämta datumet eller tiden, måste du manuellt plocka ut rätt värden från objektet. Dessutom kan olika tolkningar av datumformat och tidszoner påverka hur det aktuella datumet och tiden visas. Det är viktigt att ha detta i åtanke beroende på vad du använder det för.

## Se även

- [TypeScript Dokumentation](https://www.typescriptlang.org/docs/)
- [JavaScript Date Reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)