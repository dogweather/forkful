---
title:    "TypeScript: Att hämta aktuellt datum"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
I denna bloggpost kommer vi att utforska hur man kan få den aktuella datumen i TypeScript. Att kunna få den aktuella datumen är en viktig funktion för många programmerare eftersom det ger möjlighet att skapa dynamiska och relevanta funktioner i sina program.

## Hur man gör
För att få den aktuella datumen i TypeScript, behöver vi använda Date-objektet som är inbyggt i JavaScript. Vi deklarerar ett nytt Date-objekt och använder sedan de inbyggda funktionerna för att hämta aktuellt datum, tid, veckodag och månad.

```TypeScript
let aktuelltDatum = new Date();

console.log(aktuelltDatum); // Output: Wed Oct 13 2021 23:47:09 GMT+0200 (central europeisk sommartid)

console.log(aktuelltDatum.getFullYear()); // Output: 2021

console.log(aktuelltDatum.getMonth()); // Output: 9 (på grund av indexering börjar månaderna på 0)

console.log(aktuelltDatum.getDate()); // Output: 13

console.log(aktuelltDatum.getDay()); // Output: 3 (tisdag är index 0)

console.log(aktuelltDatum.getHours()); // Output: 23

console.log(aktuelltDatum.getMinutes()); // Output: 47
```

Det finns också möjlighet att ange specifika datum och tider i Date-objektet och få den aktuella datumen baserat på det.

```TypeScript
let specifiktDatum = new Date(2022, 0, 1); // 1 januari 2022

console.log(specifiktDatum); // Output: Sat Jan 01 2022 00:00:00 GMT+0100 (centraleuropeisk normaltid)

console.log(specifiktDatum.getDay()); // Output: 6 (lördag är index 6)
```

## Djupdykning
Det finns flera användbara funktioner i Date-objektet som kan hjälpa till att manipulera och formatera datumen på olika sätt. Till exempel kan man använda `.toLocaleDateString()` för att få en läsbar version av det aktuella datumet.

```TypeScript
console.log(aktuelltDatum.toLocaleDateString("sv-SE")); // Output: 13/10/2021
```

Man kan också använda `.valueOf()` för att få antalet millisekunder sedan 1 januari 1970, vilket är användbart för att jämföra olika datum.

```TypeScript
let förstaJanuari1970 = new Date(1970, 0, 1);

console.log(aktuelltDatum.valueOf()); // Output: 1634166429587

console.log(förstaJanuari1970.valueOf()); // Output: 0
```

Det finns många fler funktioner och metoder i Date-objektet som kan utforskas och användas för att få den aktuella datumen på olika sätt.

## Se även
- [Date-objektet i TypeScript dokumentationen](https://www.typescriptlang.org/docs/handbook/datetime.html)
- [Komplett referens för Date-objektet](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)