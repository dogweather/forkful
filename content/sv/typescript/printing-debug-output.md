---
title:                "Utmatning av feldiagnostik"
html_title:           "TypeScript: Utmatning av feldiagnostik"
simple_title:         "Utmatning av feldiagnostik"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva debug-utmatning kan hjälpa dig att felsöka och förstå vad som händer i din kod. Det är ett bra sätt att hitta och lösa problem i ditt TypeScript-projekt.

## Hur man gör

För att skriva ut debug-utmatning i TypeScript, använder du en inbyggd funktion som heter `console.log()`. Detta tar en parameter som är det du vill skriva ut. Här är ett exempel:

```TypeScript
console.log("Hej från debug-utmatning!");
```

När du kör ditt kod kommer du att se följande i din konsol:

```
Hej från debug-utmatning!
```

Det är så enkelt det är att skriva ut debug-utmatning! Du kan också använda `console.log()` för att skriva ut variabler och objekt. Till exempel:

```TypeScript
let nummer = 42;
console.log("Det valda numret är: ", nummer);
```

Detta kommer att skriva ut följande i din konsol:

```
Det valda numret är: 42
```

Du kan också använda strängar med variabler som du vill skriva ut. Till exempel:

```TypeScript
let namn = "Sofia";
console.log(`Hej ${namn}, välkommen till din kods debug-utmatning!`);
```

Detta kommer att skriva ut följande i din konsol:

```
Hej Sofia, välkommen till din kods debug-utmatning!
```

Det finns många andra sätt att använda `console.log()` för att skriva ut debug-utmatning på. Se till att utforska och prova olika sätt för att hitta det som passar bäst för ditt specifika behov.

## Deep Dive

Att skriva ut debug-utmatning är en kraftfull teknik som kan hjälpa dig att förstå ditt TypeScript-projekt bättre. Genom att använda `console.log()` kan du se värden på variabler och objekt vid olika punkter i din kod. Detta kan hjälpa dig att identifiera och lösa problem som uppstår.

Du kan också använda `console.log()` för att debugga asynkron kod. Istället för att använda en debugger, kan du skriva ut värden på olika variabler och se hur de förändras när din asynkrona kod körs.

Se också: 

- [Documentation on console.log()](https://developer.mozilla.org/en-US/docs/Web/API/Console/log)
- [Using Debugging Techniques in TypeScript](https://blog.rangle.io/debugging-techniques-in-typescript/)