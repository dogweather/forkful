---
title:                "Konvertera en sträng till gemener"
html_title:           "Javascript: Konvertera en sträng till gemener"
simple_title:         "Konvertera en sträng till gemener"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Det finns många olika situationer där det kan vara användbart att konvertera en sträng till gemener (lower case) i Javascript. Byd den korrekta formen av ett ord till det korresponderande ordet i gemener, tillåter det en enkel sökning och jämförelse av strängar, samt underlättar läsbarheten av kod.

## Så här gör du
För att konvertera en sträng till gemener i Javascript, används metoden `toLowerCase()` på den aktuella strängen. Nedan finns ett exempel på kod som visar enkel användning av denna metod:

```Javascript
let str = "HEJ";
let convertedStr = str.toLowerCase();
console.log(convertedStr);
```

Det ovanstående exemplet kommer att skriva ut "hej" i konsolen, då strängen "HEJ" har konverterats till gemener. Det är också möjligt att direkt tillämpa `toLowerCase()` på en sträng som en del av en strängoperation, som så:

```Javascript
let str = "Hej";
let modifiedStr = str + " världen".toLowerCase();
console.log(modifiedStr);
```

I detta fall skulle konsolen skriva ut "Hej världen", där "världen" har konverterats till gemener. Det är också värt att notera att metoden `toLowerCase()` returnerar en helt ny sträng, så den ursprungliga strängen bibehålls i sin ursprungliga form.

## Djupdykning
Vid konvertering till gemener, tar `toLowerCase()`-metoden hänsyn till det aktuella språket i koden. Detta betyder att bokstäver som har olika former i olika språk (som till exempel "I" och "ı" i engelska och turkiska) kommer att omvandlas korrekt till gemener enligt det aktuella språket. Om det inte finns ett specifikt språk angivet i JavaScript-koden, används vanligtvis standardspråket för webbläsaren.

Det är också värt att nämna att vissa specialtecken och icke-latinbaserade bokstäver kanske inte konverteras korrekt till gemener. Därför är det alltid viktigt att testa koden noggrant och se till att det önskade resultatet uppnås.

## Se också
- [MDN webbdocs - String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [W3Schools - Javascript String toLowerCase() Method](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
- [JavaScript.info - Changing the register](https://javascript.info/change-case)