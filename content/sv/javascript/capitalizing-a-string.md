---
title:                "Stor bokstavering av en sträng"
html_title:           "Javascript: Stor bokstavering av en sträng"
simple_title:         "Stor bokstavering av en sträng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & varför?
Att kapitalisera en sträng i Javascript betyder att göra den första bokstaven i strängen stor. Detta är en vanlig praxis för att göra texten mer läsbar och för att hålla en enhetlig stil i koden.

## Hur gör man:
Du kan kapitalisera en sträng på flera olika sätt i Javascript. Ett enkelt sätt är att använda inbyggda metoder som `toUpperCase()` och `charAt()`. Se exempel nedan:
```Javascript
let namn = "anna";
console.log(namn.charAt(0).toUpperCase() + namn.slice(1)); //Output: Anna
```

En annan metod är att använda regex (regular expressions) för att hitta och ersätta den första bokstaven i en sträng. Se exempel nedan:
```Javascript
let mening = "jag gillar javascript";
console.log(mening.replace(/^\w/, c => c.toUpperCase())); //Output: Jag gillar javascript
```

## Djupdykning:
Kapitalisering av text har varit en viktig del av programmering sedan de tidiga dagarna av datorer. En tidig standard var att alla variabler i ett program skulle skrivas i stora bokstäver för att göra det lättare att läsa och förstå koden. Idag är detta mer en fråga om personlig stil och preferens.

Det finns också andra metoder för att kapitalisera en sträng i Javascript, såsom att använda en tredjepartsbibliotek eller att skapa en egen funktion. Det viktiga är att välja en metod som passar din kod och din stil.

## Se även:
- [String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [String.prototype.charAt()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [Regular Expressions in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)