---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

---

## Vad & Varför?
Att parsa ett datum från en sträng innebär att vi omvandlar stränginformationen till ett datumobjekt i kod. Vi gör det för att vi enkelt ska kunna manipulera, jämföra och visa våra datum på effektivaste sätt.

---

## Hur Gör Man:
Låt oss se hur vi kan parsa ett datum från en sträng i TypeScript. Använda `new Date(string)`.

```TypeScript
let dateStr = "2021-03-01";
let dateParsed = new Date(dateStr);
console.log(dateParsed);
```
Detta kommer att skriva ut en `Date` objekt.

---

## Ner i Djupet
1. Historisk kontext: Vi har alltid behövt parsa datum i programmering, från dagarna av Java till dagens JavaScript och TypeScript. Det ger oss möjlighet att använda datuminformation på ett mycket mer flexibelt sätt.

2. Alternativ: Det finns bibliotek som Moment.js som tillhandahåller mer robusta lösningar för datumhantering, men dessa kan ibland vara overkill och lägga till onödigt bloat till din kod.

3. Implementeringsdetaljer: När du parsa ett datum från en sträng i TypeScript, standard `Date.parse()` funktionen används vilket är en del av JavaScript-språket. Detta kommer att automatiskt konvertera datumsträngen till ett datumobjekt som kan manipuleras i din kod.

---

## Se Även:
Här är några länkar till relaterade resurser för vidare läsning:

1. [TypeScript officiella dokumentation](https://www.typescriptlang.org/docs/)
2. [Moment.js dokumentation](https://momentjs.com/)
3. [MDN web docs - datum och tid](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Date)