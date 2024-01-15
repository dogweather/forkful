---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Javascript: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Målet med att ta bort tecken som matchar ett visst mönster kan vara att rensa bort oönskade tecken eller formatera en sträng enligt ett visst schema.

## Så här gör du

För att ta bort tecken som matchar ett visst mönster i en sträng, kan du använda JavaScript-funktionen `replace()`. Nedan följer ett exempel på hur man kan använda denna funktion:

```Javascript
let str = "Hej! Vi är här för att lära oss om JavaScript!";
let newStr = str.replace(/[^a-z0-9 ]/gi, "");
console.log(newStr);
```
Det här kommer att ge följande utmatning:

```Javascript
Hej Vi är här för att lära oss om JavaScript
```

Låt oss nu förklara koden. Vi börjar genom att skapa en variabel `str` som innehåller en sträng som vi vill rensa. Därefter skapar vi en ny variabel `newStr` som tilldelas värdet av `str.replace()`. Inom parantesen anger vi det mönster som vi vill matcha och därefter vad vi vill att det ska ersättas med, i det här fallet en tom sträng. I slutet ser vi att vi använder flaggan `gi` som betyder att vi vill att det ska vara en global matchning samt att det inte ska vara skiftlägeskänsligt (`i` står för `ignore case`). Genom att logga värdet av `newStr` till konsolen ser vi att alla tecken som inte är bokstäver eller siffror, samt mellanslag, har tagits bort från strängen.

Om du vill ändra vilka tecken som ska tas bort kan du enkelt byta ut eller lägga till i mönstret inom `replace()`-metoden. Till exempel, om du bara vill ta bort siffror från en sträng, kan du använda följande mönster: `/\d/g`.

## Djupdykning

Funktionen `replace()` är en del av JavaScript-objektet `String` och kan användas för att manipulera strängar på olika sätt. Den tar två parametrar: det mönster som ska matchas och vad som ska ersättas med.

Mönstret kan uttryckas antingen som en sträng eller genom att använda reguljära uttryck (RegExp). I vårt exempel använde vi RegExp för att kunna ta bort alla oönskade tecken, men det är också vanligt att man använder `replace()` med en sträng som första parameter. Om det mönster du vill matcha bara är en enkel sträng kan du även använda metoden `replace()` direkt på strängen utan att behöva skapa ett RegExp-objekt först.

`replace()`-metoden är inte bara begränsad till att byta ut eller ta bort tecken. Genom att använda en konstgjord variabel kallad "matcher" kan du till exempel referera till det matchade mönstret och använda det på olika sätt. Om du vill lära dig mer om detta så kan du kolla in länkarna i avsnittet "Se även" nedan.

## Se även

Här är några användbara resurser för att lära dig mer om `replace()`-metoden och reguljära uttryck:

- [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3Schools - JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
- [Regular-Expressions.info - JavaScript Regex Cheat Sheet](https://www.regular-expressions.info/javascript.html)