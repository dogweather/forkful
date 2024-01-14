---
title:    "Javascript: Generering av slumpmässiga tal"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en vanlig funktion inom programmering. Det används för att skapa variation och inte låta resultatet bli förutsägbart. Det kan också användas för att simulera verkliga scenarier eller som en del av krypteringsalgoritmer.

## Så här gör du

Generering av slumpmässiga nummer är ganska enkelt i JavaScript. Här är ett exempel på hur man kan skapa ett slumpmässigt heltal mellan 1 och 10:

```Javascript
let randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber);
```

Koden ovan använder sig av Math-objektet som finns inbyggt i JavaScript. Genom att använda sig av `Math.random()` funktionen kan vi skapa ett decimaltal mellan 0 och 1. Genom att sedan multiplicera med 10 och avrunda nedåt med `Math.floor()` får vi ett heltal mellan 0 och 9. Genom att sedan lägga till 1 får vi ett heltal mellan 1 och 10.

Det finns också andra metoder för att generera slumpmässiga nummer i JavaScript, till exempel `Math.round()` och `Math.ceil()`. Det är viktigt att tänka på att dessa metoder inte genererar helt slumpmässiga nummer utan använder sig av förutbestämda algoritmer. För att få mer slumpmässiga nummer kan man använda sig av externa bibliotek eller API:er.

## Djupdykning

JavaScript använder sig av en vanlig algoritm för att generera slumpmässiga nummer, kallad "Linear Congruential Algorithm". Det är en relativt enkel metod för att generera nummer men det finns vissa begränsningar. Till exempel är det svårt att garantera att alla tal blir slumpmässiga och att det inte uppstår mönster i följderna av nummer.

Det finns också andra faktorer som kan påverka resultatet av slumpmässiga nummer, till exempel användning av samma frö (seed) eller vilken webbläsare som används. Därför är det viktigt att förstå begränsningarna och användningsområdena för slumpmässiga nummer i JavaScript.

## Se även

- [Math-objektet i JavaScript](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Math)
- [Slumpmässiga nummer i Javascript](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Math/random)