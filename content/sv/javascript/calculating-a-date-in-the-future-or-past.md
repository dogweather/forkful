---
title:                "Javascript: Beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##Varför

Att kunna beräkna ett datum i framtiden eller förflutet kan vara användbart för att skapa interaktiva kalendrar, planera evenemang eller hålla koll på åldersberäkningar i en applikation.

##Hur man gör det

Det finns en enkel funktion i Javascript som heter `getDate()` som beräknar dagens datum. Men om vi vill beräkna ett datum i framtiden eller förflutet, behöver vi använda både `getDate()` och `setDate()` funktioner. Här är ett exempel på hur man kan göra det:

```javascript
// Skapa ett nytt Date objekt för dagens datum
const today = new Date();

// Beräkna datumet en vecka framåt
const nextWeek = new Date();
// Använd `setDate()` funktionen för att ändra datumet till en vecka framåt från dagens datum
nextWeek.setDate(today.getDate() + 7);

// Beräkna datumet en månad bakåt
const lastMonth = new Date();
// Använd `setDate()` funktionen för att ändra datumet till en månad bakåt från dagens datum
lastMonth.setDate(today.getDate() - 30);

// Skriv ut datumen
console.log(`Dagens datum: ${today}`);
console.log(`Datumet om en vecka: ${nextWeek}`);
console.log(`Datumet för en månad sedan: ${lastMonth}`);
```

Output:

```
Dagens datum: Tue Dec 01 2020 18:16:34 GMT+0100 (Central European Standard Time)
Datumet om en vecka: Tue Dec 08 2020 18:16:34 GMT+0100 (Central European Standard Time)
Datumet för en månad sedan: Sat Oct 31 2020 18:16:34 GMT+0100 (Central European Standard Time)
```

##Djupdykning

`getDate()` funktionen returnerar en numerisk representation av dagen i månaden (1 till 31). När vi använder `setDate()` funktionen och till exempel adderar 7 för att få datumet en vecka framåt, så kommer den automatiskt att justera månaden och året om det är nödvändigt för att få ett giltigt datum. Detta betyder att om dagen är den 31 och vi lägger till 7, kommer månaden automatiskt att justeras till nästa månad. På samma sätt kommer året att justeras om månaden är december och vi adderar mer än 31 dagar.

##Se även

* [Date Objekt i Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
* [getDate() funktionen](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getDate)
* [setDate() funktionen](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setDate)