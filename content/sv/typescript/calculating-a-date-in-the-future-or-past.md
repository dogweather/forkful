---
title:                "Beräkna ett datum i framtiden eller förfluten tid"
html_title:           "TypeScript: Beräkna ett datum i framtiden eller förfluten tid"
simple_title:         "Beräkna ett datum i framtiden eller förfluten tid"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##_ Vad & Varför?

Att beräkna ett datum i framtiden eller förflutna handlar om att lägga till eller dra ifrån specifika tidsintervaller från ett givet datum. Detta är viktigt för programmerare för att hantera olika tidsrelaterade scenarier, som schemaläggning av händelser eller jämförelser av datum.

##_ Hur man gör:

I TypeScript kan du skapa en ny `Date` instans och sedan använda metoden `setDate` för att ändra den. Här är ett exempel:

```TypeScript
let today = new Date();
let futureDate = new Date();
futureDate.setDate(today.getDate() + 5);
console.log(futureDate);
```

Detta program kommer att skriva ut ett datum fem dagar i framtiden.

##_ Fördjupning

Att räkna ut framtidiga och förflutna datum är inte en ny idé, utan har rötter i gammal tidmätningsteori. I modern programmering finns det flera metoder för att göra detta, och vilken metod du väljer beror på ditt specifika behov. Det kan till exempel vara bättre att använda bibliotek som date-fns eller moment.js om ditt projekt har mycket komplexa datumoperationer.

När du beräknar datum gör JavaScript (och TypeScript) justeringar för månader med olika antal dagar och skottår automatiskt. Om du till exempel säger `setDate(32)` på en datuminstans från januari, får du den första februari.

##_ Se även

- [MDN Web Docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [date-fns](https://date-fns.org/)
- [moment.js](https://momentjs.com/)