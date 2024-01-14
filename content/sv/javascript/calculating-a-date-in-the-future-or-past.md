---
title:                "Javascript: Beräkna ett datum i framtiden eller förfluten tid"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att beräkna ett datum i framtiden eller i det förflutna kan vara mycket användbart i olika programmeringsprojekt eller webbapplikationer. Det kan till exempel användas för att visa födelsedagar eller deadlines, eller för att planera scheman och evenemang. Det är en enkel men kraftfull funktion som kan förenkla många olika uppgifter.

## Så här gör du

För att beräkna ett datum i framtiden eller förflutna behöver vi först förstå några grundläggande koncept. Vi ska använda oss av Javascripts inbyggda `Date`-klass för att manipulera datum och tid.

För att beräkna ett datum i framtiden kan vi använda `Date`-klassens `setDate()`-metod. Nedan följer ett exempel på hur du kan ange ett datum 30 dagar framåt från dagens datum:

```Javascript
let today = new Date(); // Skapar ett nytt datumobjekt för dagens datum
let futureDate = today.setDate(today.getDate() + 30); // Lägger till 30 dagar till dagens datum

console.log(futureDate); // Output: 1585847293730
```

I koden ovan använder vi `setDate()` för att sätta ett nytt datum för `futureDate`-objektet. Vi tillhandahåller också ett värde för variabeln `today` genom att skapa ett nytt datumobjekt och sedan lägga till 30 dagar till det med hjälp av `getDate()`-metoden.

På samma sätt kan vi också beräkna ett datum i förflutna genom att subtrahera antal dagar från dagens datum:

```Javascript
let today = new Date(); // Skapar ett nytt datumobjekt för dagens datum
let pastDate = today.setDate(today.getDate() - 7); // Subtraherar 7 dagar från dagens datum

console.log(pastDate); // Output: 1585276870598
```

Som du kan se i exemplet ovan sätter vi också ett nytt datum för `pastDate` genom att använda `setDate()`-metoden och subtrahera 7 dagar från dagens datum.

##djupdykning

Det finns många fler inbyggda metoder i `Date`-klassen som du kan använda för att beräkna datum i framtiden och förflutna. Till exempel kan du använda `setMonth()`-metoden för att beräkna ett datum i framtiden eller förflutna baserat på månader istället för dagar.

Du kan också använda dig av andra funktioner och bibliotek, som moment.js, för att förenkla beräkning av datum och tid i Javascript.

## Se även

- [Mozilla Developer Network - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - JavaScript Date Object](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Moment.js](https://momentjs.com/)