---
title:    "Javascript: Skrivning till standardfel"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är en viktig del av felsökning i programvara. Genom att skicka felmeddelanden till standard error istället för standard out, kan utvecklare enkelt identifiera och åtgärda problem i koden.

## Hur man gör

Det är enkelt att skriva till standard error i Javascript. Man kan använda metoden `console.error()` för att skriva ut felmeddelanden till standard error streamen. Här är ett exempel:

```Javascript
try {
  // Kod som kan orsaka fel
  throw "Ett fel har uppstått";
} catch (error) {
  // Skriver ut felmeddelandet till standard error
  console.error(error);
}
```

Detta kommer att skriva ut "Ett fel har uppstått" till standard error streamen och hjälpa dig att identifiera var felet uppstod.

## Djupdykning

Skriva till standard error är också bra för att skilja mellan olika typer av utdata i en process. Genom att använda standard error för felmeddelanden och standard out för normal utdata, blir det lättare att automatiskt hantera eventuella fel som kan uppstå.

Det är också viktigt att notera att standard error inte bara är till för felmeddelanden, utan kan också användas för att logga viktig information som kan hjälpa till med felsökning och förbättring av koden.

## Se också

Här är några länkar där du kan läsa mer om att skriva till standard error i Javascript:

- [Node.js Dokumentation: Standard Streams](https://nodejs.org/api/process.html#process_process_stdin)
- [Mastering Node.js: Understanding Standard Streams](https://medium.com/@purjus/understanding-standard-streams-in-node-js-b9f03f746123)
- [Stack Overflow: Difference between console.log and console.error](https://stackoverflow.com/questions/51250783/difference-between-console-log-and-console-error)