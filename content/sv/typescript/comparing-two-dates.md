---
title:                "Jämförande av två datum"
html_title:           "TypeScript: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum är en vanlig uppgift inom programmering, särskilt när man arbetar med tidsrelaterade applikationer eller funktioner. Genom att jämföra datum kan man få information om vilket datum som kommer först, om de är lika och mycket mer. Det är alltså viktigt att ha grundläggande kunskap om hur man jämför datum i TypeScript.

## Hur man gör
För att jämföra två datum i TypeScript kan du använda kommandot `new Date()` för att skapa ett datumobjekt för varje datum som du vill jämföra. Sedan kan du använda jämförelseoperatorerna (`<`, `>`, `==`) för att jämföra datumen.

Exempel 1:
```TypeScript
let date1 = new Date("2020-01-01");
let date2 = new Date("2020-01-05");

if (date1 < date2) {
  console.log(date1 + " är tidigare än " + date2);
} else if (date1 > date2) {
  console.log(date1 + " är senare än " + date2);
} else {
  console.log(date1 + " och " + date2 + " är samma datum.");
}
```
Output: 2020-01-01 är tidigare än 2020-01-05.

Exempel 2:
```TypeScript
let today = new Date();
let deadline = new Date("2021-03-15");

if (today < deadline) {
  console.log("Du har fortfarande tid att slutföra uppgiften!");
} else {
  console.log("Tiden har gått ut, hoppas du slutförde uppgiften i tid.");
}
```
Output: Du har fortfarande tid att slutföra uppgiften!

## Deep Dive
När man jämför två datum är det viktigt att förstå att datum i TypeScript representeras som millisekunder efter 1 januari 1970. Detta innebär att ju senare datumet ligger, desto större blir dess millisekundvärde. Därför fungerar jämförelseoperatorerna för datum på samma sätt som för numeriska värden.

Det finns också andra metoder som kan användas för att jämföra datum i TypeScript, såsom `getTime()` och `getTimezoneOffset()`. Dessa kan vara användbara beroende på vilken typ av jämförelse som behövs.

## Se också
- [Date - TypeScript Documentation](https://www.typescriptlang.org/docs/handbook/datetime.html)
- [JavaScript Date Objects - W3Schools](https://www.w3schools.com/js/js_dates.asp)
- [Comparing Dates in JavaScript - Stack Overflow](https://stackoverflow.com/questions/1197928/how-to-compare-two-dates-in-javascript)