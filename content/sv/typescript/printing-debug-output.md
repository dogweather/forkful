---
title:    "TypeScript: Utskrift av felsökningsutdata"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Varför

I programmering är det ofta nödvändigt att felsöka koden för att hitta och åtgärda eventuella fel. En bra metod för felsökning är att skriva ut debug-meddelanden, eller "debug output" på engelska. Detta kan ge viktig information som kan hjälpa dig att förstå vad som händer i koden och varför det eventuellt inte fungerar som förväntat.

## Hur man gör det

I TypeScript finns det flera sätt att skriva ut debug-meddelanden, beroende på din specifika situation. Ett sätt är att använda loggningsfunktionen "console.log()", som skriver ut ett meddelande i webbläsarens utvecklarverktyg eller i terminalen om du kör ditt program utanför webbläsaren. Här är ett enkelt exempel på hur du kan använda "console.log()" i TypeScript:

```
TypeScript
var num1: number = 5;
var num2: number = 10;
console.log("Summan av " + num1 + " och " + num2 + " är " + (num1 + num2));
```

Detta kommer att skriva ut "Summan av 5 och 10 är 15" i ditt utvecklarverktyg eller terminal. Detta kan hjälpa dig att kontrollera att dina variabler har rätt värden och att din kod fungerar som den ska.

En annan metod är att använda "debugger", vilket är ett inbyggt kommando i webbläsaren som pausar koden och tillåter dig att steg-för-steg gå igenom den. Detta kan vara särskilt användbart när du felsöker komplicerade problem eller när du inte vet exakt var i koden problemet uppstår.

## Djupdykning

Att skriva ut debug-meddelanden är en viktig del av felsökning i TypeScript, men det är också viktigt att inte överanvända det. Att ha för många debug-meddelanden kan göra koden rörig och svår att läsa. Det är därför viktigt att vara selektiv och bara skriva ut det mest relevanta och viktiga informationen.

En annan viktig aspekt är att ta bort debug-kod innan du publicerar din kod eller delar den med andra. Detta för att undvika att onödig information visas för användare, vilket kan vara en säkerhetsrisk.

## Se även

- [TypeScript Handbook: Debugging](https://www.typescriptlang.org/docs/handbook/declaration-files/deep-dive.html)
- [Debugging TypeScript in Chrome and Visual Studio Code](https://blog.logrocket.com/debugging-typescript-in-chrome-and-visual-studio-code/)
- [Using the JavaScript console](https://developer.mozilla.org/en-US/docs/Tools/JavaScript_console) (du kan använda samma metoder i TypeScript)
- [Debugging TypeScript with breakpoints](https://stackoverflow.com/questions/16598467/debug-typescript-with-breakpoints)