---
title:                "TypeScript: Beräkning av datum i framtiden eller i det förflutna"
simple_title:         "Beräkning av datum i framtiden eller i det förflutna"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

I den moderna världen av teknik och programmering är det ibland nödvändigt att kunna beräkna ett datum i framtiden eller det förflutna. Detta kan vara användbart för att skapa dynamiska tidsbaserade funktioner eller för att visa rätt information baserat på ett visst datum. Med TypeScript är detta enkelt att göra med hjälp av inbyggda metoderna och funktionerna. Läs vidare för att lära dig hur du kan beräkna ett datum i framtiden eller det förflutna i TypeScript.

## Så här gör du

För att beräkna ett datum i framtiden eller det förflutna i TypeScript behöver vi först använda oss av Date-objektet. Detta objekt har inbyggda metoder som vi kan använda för att beräkna olika datum baserat på ett befintligt datum.

Låt oss säga att vi vill beräkna ett datum som är 2 veckor framåt från idag. Detta kan göras genom att använda metoden `setDate()` för att lägga till 14 dagar till det aktuella datumet.

```TypeScript
let datum = new Date();

datum.setDate(datum.getDate() + 14); // lägg till 14 dagar
console.log(datum); // output: 2021-10-10T19:12:53.500Z
```

På samma sätt kan vi också beräkna ett datum i det förflutna genom att använda `setDate()` med ett negativt värde.

```TypeScript
let datum = new Date();

datum.setDate(datum.getDate() - 7) // dra av 7 dagar
console.log(datum); // output: 2021-09-27T19:12:53.500Z
```

Vi kan också använda andra inbyggda metoder som `setFullYear()` och `setMonth()` för att beräkna datum i framtiden eller det förflutna baserat på år och månad.

```TypeScript
let datum = new Date();

datum.setFullYear(datum.getFullYear() + 1); // lägg till ett år
datum.setMonth(datum.getMonth() + 3); // lägg till 3 månader
console.log(datum); // output: 2022-01-01T19:12:53.500Z
```

## Deep Dive

Om vi vill ha mer kontroll över våra beräkningar kan vi också använda andra metoder såsom `setHours()` och `setMinutes()` för att ställa in specifika tider på dagen.

```TypeScript
let datum = new Date();

datum.setDate(datum.getDate() + 7); // beräkna datumet 7 dagar framåt
datum.setHours(14, 30, 0); // ställ in tiden på 14:30:00
console.log(datum); // output: 2021-10-04T12:30:00.500Z
```

Det är också viktigt att notera att Date-objektet använder sig av tidszoner, så om du vill ha ett specifikt datum i en viss tidszon bör du också använda metoden `setHours()` för att justera tiden.

## Se också

- [Date API Dokumentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Officiell Dokumentation](https://www.typescriptlang.org/docs/)
- [Denna artikel på engelska](https://www.example.com)