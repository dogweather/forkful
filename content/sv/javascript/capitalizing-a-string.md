---
title:    "Javascript: Att göra en sträng med stora bokstäver"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Varför

Att koda är som ett matematiskt pussel där man sätter ihop bitar för att lösa ett problem. Att capitalisera en sträng kan vara en del av detta pussel och kan hjälpa till att skapa mer läsbara och förståeliga resultat i dina program.

## Hur To

För att capitalisera en sträng i Javascript, kan vi använda oss av inbyggda funktioner som `toUpperCase()` eller `charAt()`. Här är ett exempel på kod som tar en sträng och gör den helt capitals:

```Javascript
let namn = "svenska"

namn = namn.toUpperCase();

console.log(namn);
```

Output:

`SVENSKA`

Vi kan också använda oss av en `for` loop för att loopa igenom varje bokstav i strängen och använda `charAt()` för att ändra första bokstaven till en capital och sedan lägga till resten av bokstäverna. Här är ett annat exempel på kod:

```Javascript
let ord = "javascript"

let nyStrang = "";
for (let i = 0; i < ord.length; i++) {
    if (i === 0) {
        nyStrang += ord[i].toUpperCase();
    } else {
        nyStrang += ord[i];
    }
}

console.log(nyStrang);
```

Output:

`Javascript`

## Deep Dive

När vi capitaliserar en sträng, förändrar vi varje bokstav till en stor bokstav. Detta hjälper till att skapa en mer enhetlig och lättläst sträng, speciellt när vi har andra ord som är capitalize. Detta gör det också enklare att söka igenom och söka efter en sträng i vårt program.

Några saker att tänka på när man capitaliserar en sträng är att det här endast ändrar bokstäver och inte andra specialtecken eller siffror. Det är också viktigt att tänka på att vissa språk har olika regler för capitalisering, till exempel att det kan finnas accenter eller diakritiska tecken som också behöver förändras.

## Se också

- [MDN - String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN - String.prototype.charAt()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)