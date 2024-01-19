---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva ut debug output, eller debuggning, är en del av programmeringsprocessen där du undersöker och justerar din kod för att fixa eventuella fel. Genom att använda denna teknik kan programmerare effektivt identifiera och rätta till problem i koden.

## Hur gör man:

Javascript tillhandahåller flera metoder för att skriva ut debug output. Kolla på den här koden:

```Javascript
console.log('Hello, Debugging!');
```

Utför koden ovan och det kommer att skriva ut texten 'Hello, Debugging!' i din webbläsares konsol. Mycket enkelt, eller hur?

```Javascript
let a = 5;
let b = 10;
console.log('Addition: ', a + b);
```

Utför koden ovan och du kommer att se 'Addition: 15' i konsolen. 

## Djupdykning:

**Historisk Kontext:** Att skriva ut debug output i JavaScript har varit möjligt sedan det först skapades. Det har hjälpt tusentals programmerare att lösa fel i deras kod.

**Alternativ:** Förutom `console.log()`, JavaScript erbjuder också andra metoder för att skriva ut debug output, såsom `console.info()`, `console.warn()`, och `console.error()`. Var och en presenterar debug output på ett annat sätt för att särskilja olika typer av meddelanden.

**Implementeringsdetaljer:** `console.log()` är en del av Web API som är tillgänglig i bland annat webbläsare. Detta innebär att metoden kanske inte fungerar på liknande sätt på andra plattformar, som Node.js.

## Se Även:

[JavaScript Console](https://developer.mozilla.org/sv-SE/docs/Web/API/Console)

[JavaScript Debugging](https://developer.mozilla.org/sv-SE/docs/Learn/JavaScript/First_steps/What_went_wrong)

Observera, att lära sig att skriva ut debug output i din kod är bara början. För att bli en effektiv Javascript-programmerare, bör man bli bra på problemlösning och förståelse för hur koden fungerar på djupet.