---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka ett datum från en sträng innebär att omvandla en textrepresentation av ett datum till ett datumobjekt som kan interageras med. Programmers gör detta för att möjliggöra beräkningar och manipulationer som att lägga till dagar, jämföra datum och mer.

## Så här gör du:

För att tolka ett datum från en sträng, använder vi Javascripts inbyggda Date konstruktor.

```Javascript
let strDate = "2021-11-26";
let parsedDate = new Date(strDate);
console.log(parsedDate);
```

När du kör ovanstående kod får du utskriften

```Javascript
2021-11-26T00:00:00.000Z
```

## Djupdykning

Historiskt sett fanns det ingen standardiserad metod för att hantera datumsträngar i Javascript. Tidigare versioner av ECMA-standarden specificerade inte exakt format som datumsträngar måste följa, vilket ledde till tolkningsinkonsekvenser över olika webbläsare.

Men med ECMAScript 5 introducerades ett standardiserat format - ISO-8601 - vilket är den vanligaste metoden för att tolka datumsträngar idag.

Alternativt kan du skriva egna funktioner för att tolka datum baserat på ett specifikt format. Ett annat alternativ är att använda bibliotek som Moment.js vilket erbjuder avancerade datum- och tidshantering.

Detaljerna i implementationen av `Date` konstruktorn ligger djupt i Javascript-motorns kärna. Men kortfattat kommer Javascript att tolka din sträng från vänster till höger, och söka efter ett erkänt datum- eller tidsformat.

## Se även

För mer information om datum och tider i JavaScript, kolla in dessa resurser.

- [Mozilla Developer Network's guide on Date and Time Strings](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)
- [Handling dates in JavaScript](https://flaviocopes.com/javascript-dates/)
- [Moment.js library](https://momentjs.com/)