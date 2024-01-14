---
title:    "TypeScript: Beräkna en datum i framtiden eller det förflutna"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Varför

Att beräkna datum i framtiden eller förflutet kan vara till nytta i många olika situationer. Det kan hjälpa till att planera möten, resor eller att få en överblick över viktiga händelser i ens liv.

# Hur man gör det

För att beräkna ett datum i framtiden eller förflutet kan man använda sig av TypeScript Date-klassen och dess metoder. Nedan följer ett exempel på en funktion som tar in ett antal dagar och returnerar det beräknade datumet:

```TypeScript
function calculateDate(days: number): Date {
  let currentDate = new Date(); // Skapar ett nytt Date-objekt som representerar dagens datum
  currentDate.setDate(currentDate.getDate() + days); // Använder setDate-metoden för att lägga till det angivna antalet dagar till currentDate
  return currentDate;
}

console.log(calculateDate(7)); // Output: 2021-08-11T23:01:59.856Z
```

I detta exempel används setDate-metoden för att ändra värdet på dagarna i det existerande Date-objektet. Detta gör att vi kan beräkna ett datum x antal dagar från dagens datum.

Det finns också flera andra användbara metoder i Date-klassen, såsom `getFullYear()` (returnerar året i ett Date-objekt), `getMonth()` (returnerar månaden i ett Date-objekt) och `getDay()` (returnerar vilken dag i veckan ett Date-objekt representerar).

# Djupdykning

För att beräkna ett datum i förflutet eller framtiden kan man även använda sig av olika bibliotek som tillhandahåller mer avancerade funktioner och möjligheter. Ett exempel på ett sådant bibliotek är Moment.js, som erbjuder enkla och användarvänliga metoder för beräkning av datum.

Djupdykningar i dessa bibliotek kan vara användbara för att lära sig mer om de olika datumenheter och format som finns, eller för att få en bättre förståelse för hur man kan hantera tidszoner och sommartider.

# Se också

- [Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)