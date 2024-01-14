---
title:    "Javascript: Utskrift av felsökningsresultat"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Varför

Att skriva kod kan ibland vara en utmaning, speciellt när det uppstår buggar som är svåra att hitta. Då kan det vara till stor hjälp att använda sig av debuggning och att skriva ut felmeddelanden för att lättare hitta och åtgärda problemen.

## Så här gör du

För att skriva ut debugmeddelanden i din Javascript-kod kan du använda console.log() funktionen. Detta tillåter dig att skriva ut information om variabler, värden och meddelanden direkt i konsolen. Här är ett exempel:

```Javascript
let num1 = 5;
let num2 = 10;
let result = num1 + num2;
console.log("Resultatet är " + result);
```

Detta kommer att skriva ut "Resultatet är 15" i konsolen, vilket hjälper dig att kontrollera att dina variabler och uträkningar är korrekta. Du kan även använda console.log() för att utvärdera if-satser och loopar, vilket kan ge dig värdefull information om vad som händer i din kod.

## Djupdykning

Utöver att enkelt skriva ut värden och meddelanden i konsolen, kan du även använda andra sätt att debugga din Javascript-kod. Ett annat alternativ är att använda debugger som kan sättas in direkt i din kod för att pausa körningen och låta dig stega igenom koden rad för rad. Du kan också använda en webbläsarens utvecklarverktyg för att inspektera dina variabler och kontrollera under körning.

## Se även

- [Javascript debugging dokumentation hos Mozilla](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/debugger)
- [En guide till Javascript debuggning med VS Code](https://code.visualstudio.com/docs/editor/debugging)
- [Debuggning med Google Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools/javascript)