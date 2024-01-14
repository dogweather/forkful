---
title:    "Javascript: Hitta längden på en sträng"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en grundläggande del av programmering och kan vara användbart för att lösa många olika problem.

## Hur man gör

För att hitta längden på en sträng i JavaScript kan man använda sig av metoden `length()`. Detta gör det möjligt att enkelt få reda på hur många tecken en sträng innehåller.

``` Javascript
let string = "Hej världen";
console.log(string.length());
// output: 11
```

Det spelar ingen roll om strängen innehåller bokstäver, tal eller specialtecken, `length()`-metoden räknar alla tecken som en del av strängen.

## Djupdykning

I JavaScript är strängar en typ av data som kan lagras och manipuleras i variabler. När man använder `length()`-metoden så räknas alla tecken i strängen, inklusive mellanslag. Om man istället vill ta bort mellanslagen och bara räkna tecken som är bokstäver eller siffror, kan man använda metoden `trim()`. Detta är användbart när man vill kontrollera längden på strängar som kommer från användarinput.

En annan intressant användning av `length()`-metoden är att kontrollera om en sträng är tom eller inte. Om `length()` returnerar 0 betyder det att strängen inte innehåller några tecken och därmed är tom.

## Se även

- [W3Schools - JavaScript String length() Method](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [MDN Web Docs - String.prototype.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)