---
title:                "Javascript: Att använda reguljära uttryck"
simple_title:         "Att använda reguljära uttryck"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför använda reguljära uttryck?

Reguljära uttryck är en kraftfull uppfinning inom webbutveckling som hjälper till att söka och matcha specifika mönster i en textsträng. Genom att använda reguljära uttryck kan du på ett effektivt sätt validera användarinput, söka igenom stora datamängder och ersätta delar av en textsträng med annan text. Det är ett oumbärligt verktyg för att hantera och manipulera text i JavaScript-kod.

## Hur man använder reguljära uttryck

För att använda reguljära uttryck i JavaScript måste du först skapa ett RegExp-objekt. Detta kan göras genom att antingen använda en reguljär uttrycksliteral med ett framåtsnedstreck (`/`) eller genom att använda RegExp-konstruktorn. Låt oss ta en titt på ett exempel som matchar en e-postadress i en textsträng:

```Javascript
let emailRegex = /([a-zA-Z0-9._-]+@[a-zA-Z0-9._-]+\.[a-zA-Z]+)/;
let text = "Kontakta mig på john@example.com för mer information.";
let match = text.match(emailRegex);
console.log(match[0]); // john@example.com
```

I exemplet ovan skapar vi ett RegExp-objekt som matchar den vanligaste typen av e-postadresser. Vi använder sedan metoden `match()` för att söka efter en matchning i `text`-variabeln och få ut en referens till själva matchen i `match`-variabeln. I vårt fall kommer detta att vara e-postadressen i textsträngen.

Men reguljära uttryck kan även användas för att göra mer avancerade matchingar och ersättningar. Till exempel kan du matcha alla ord i en textsträng som börjar med en viss bokstav eller innehåller ett visst ord. Det finns många olika reguljära uttrycksmönster att utforska och det är en bra idé att prova olika variationer för att bli bekväm med dem.

## Djupdykning i reguljära uttryck

Utöver grundläggande användning kan reguljära uttryck även användas för att kategorisera och kvantifiera mönster, vilket gör dem ännu kraftfullare. Till exempel kan du matcha en viss mängd bokstäver, siffror eller tecken med hjälp av kvantifierare som `+` (en eller fler), `*` (noll eller fler) och `?` (noll eller en). Du kan även använda karaktärsklasser som `[a-z]` för att matcha alla små bokstäver och `[0-9]` för att matcha alla siffror i en textsträng. Det finns många olika metakaraktärer och specialsekvenser som kan användas för att göra mer avancerade matchningar och ersättningar. För mer information kan du kolla in [denna guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) från Mozilla Developer Network.

## Se även

- [RegExp reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [Regular expressions for beginners](https://www.w3schools.com/js/js_regexp.asp)
- [Regular expressions cheatsheet](https://www.debuggex.com/cheatsheet/regex/javascript)