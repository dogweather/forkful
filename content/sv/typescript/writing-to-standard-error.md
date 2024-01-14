---
title:                "TypeScript: Att skriva till standardfel"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfelflödet kan vara en användbar funktion i dina TypeScript-program. Genom att skriva till standardfelflödet kan du visa meddelanden i terminalen som kan hjälpa dig att felsöka och förbättra din kod.

## Så här gör du

För att skriva till standardfelflödet i dina TypeScript-program kan du använda ```console.error()```-funktionen. Här är ett enkelt exempel:

```TypeScript
let num: number = 10;
if (num > 5) {
  console.error("Numret är större än 5!");
}
```

När detta kodblock körs kommer meddelandet "Numret är större än 5!" att skrivas till standardfelflödet i terminalen.

## Djupdykning

När du använder ```console.error()``` är det viktigt att ha i åtanke att det inte bara är avsett för felmeddelanden. Det kan också vara ett bra sätt att visa viktiga meddelanden eller för att markera särskilda händelser i ditt program.

Det är också värt att nämna att det finns andra sätt att skriva till standardfelflödet, som till exempel att använda ```process.stderr.write()```. Det är dock viktigt att förstå skillnaderna mellan dessa olika metoder och välja den som bäst passar ditt syfte.

## Se även

- [Official TypeScript Documentation - Console](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-5.html#new-features)
- [MDN Web Docs - Console](https://developer.mozilla.org/en-US/docs/Web/API/Console)