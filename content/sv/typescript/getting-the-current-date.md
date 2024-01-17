---
title:                "Få den nuvarande datumet"
html_title:           "TypeScript: Få den nuvarande datumet"
simple_title:         "Få den nuvarande datumet"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få den nuvarande datumet är en grundläggande funktion inom programmering. Det låter dig hämta och hantera det aktuella datumet och tiden för användning i dina applikationer. Detta är särskilt användbart för att hålla koll på tidsstämplar för händelser eller för att visa nuvarande datum till användare.

## Så här:
För att få det nuvarande datumet i TypeScript kan du använda Date-objektet och dess metoder. Här är ett exempel på hur du kan få det nuvarande datumet och tiden i ISO-format:
```TypeScript
let currentDate = new Date();
console.log(currentDate.toISOString()); // 2021-01-01T00:00:00.000Z
```
Du kan också använda Date-objektets andra metoder som .getDate(), .getMonth() och .getFullYear() för att få enskilda delar av datumet.

## Djupdykning:
Datumhåntering har funnits i många år inom programmering och det finns flera olika sätt att få det nuvarande datumet. Tidigare var det vanligt att använda JavaScripts Date-objekt direkt, men med TypeScript får vi tillgång till datatyper och strikta kontroller för att hantera datum.

För att få ett mer precist datum kan du använda tredjepartsbiblioteket moment.js, som ger flera olika metoder och möjligheter för att manipulera datum och tid.

## Se även:
- [Date-objektet på MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js dokumentation](https://momentjs.com/docs/)