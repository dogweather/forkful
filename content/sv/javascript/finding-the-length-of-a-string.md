---
title:                "Hitta längden på en sträng"
html_title:           "Javascript: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hitta längden på en sträng är en vanlig uppgift inom programmering. Det innebär helt enkelt att ta reda på hur många tecken en sträng innehåller. Detta är användbart för att hantera och manipulera textsträngar på olika sätt.

## Så här:

```Javascript
// Exempel 1: Hitta längden på en statisk sträng
let str = "Hello World";
console.log(str.length); // Output: 11

// Exempel 2: Hitta längden på en dynamisk sträng
let userInput = prompt("Skriv en sträng: ");
console.log(userInput.length); // Output: längden på användarens input
```

## Djupdykning:

Att hitta längden på en sträng är inte en ny eller komplicerad koncept inom programmering. Det har funnits med sedan de tidiga dagarna av programmeringsspråk som C och Fortran. Det finns också flera sätt att uppnå samma resultat.

Ett alternativ till att använda .length-metoden är att använda en for-loop och iterera över varje tecken i strängen, räkna dem under tiden. Detta kan vara användbart om man vill utföra andra operationer på tecknen samtidigt.

Implementeringen av .length-metoden kan också variera lite beroende på programmeringsspråk. I Javascript är .length en egenskap och inte en metod, så det är inget behov av parenteser efter .length.

## Se även:

- [String.length - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length) - officiell dokumentation från Mozilla.
- [String length() Function - W3Schools](https://www.w3schools.com/jsref/jsref_length_string.asp) - en tutorial med exempel och förklaringar.