---
title:                "Utskrift av felsökningsutdata"
html_title:           "Javascript: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Felsökning är en viktig del av programmering och en metod som används för att hitta och åtgärda fel i koden är att skriva ut debug-meddelanden. Detta innebär att programmeraren växelvis skriver ut vissa variabler eller värden under körningen av programmet för att kontrollera dess tillstånd och hitta eventuella fel.

## Så här gör du:
I Javascript kan du skriva ut debug-meddelanden med hjälp av inbyggda funktionen `console.log()`. Du kan helt enkelt skriva ut ett meddelande eller ett variabelvärde i koden och det kommer att visas i din webbläsares utvecklarkonsol. Till exempel:

```Javascript
console.log("Hej, världen!");
```

Detta kommer att skriva ut "Hej, världen!" i konsolen när koden körs.

Du kan också skriva ut variabler eller objekt och se dess värden i konsolen. Till exempel:

```Javascript
var nummer = 10;
console.log(nummer);
```

Detta kommer att skriva ut värdet av `nummer`-variabeln (10) i konsolen.

## Djupdykning:
Historiskt sett har debugging varit en utmaning för programmerare, särskilt i språk som inte hade inbyggda verktyg för detta ändamål. Förr i tiden använde programmerare ofta en rad olika metoder för att hitta och åtgärda fel, såsom att lägga till extra utskrifter i koden eller använda specialverktyg.

I dagens moderna språk och plattformar, som Javascript och webbläsare, är det enklare att hitta och åtgärda fel tack vare inbyggda felsökningsverktyg. I Javascript kan du till exempel använda `console.log()` för att skriva ut meddelanden utan att behöva använda externa verktyg.

Det finns även andra sätt att debugga i Javascript, såsom att använda debuggningverktyg i webbläsaren eller att använda tredjepartsbibliotek som `debug.js`.

## Se även:
- https://developer.mozilla.org/en-US/docs/Web/API/console/log
- https://www.digitalocean.com/community/tutorials/how-to-debug-node-js-with-the-built-in-debugger-and-v8-inspector
- https://github.com/visionmedia/debug