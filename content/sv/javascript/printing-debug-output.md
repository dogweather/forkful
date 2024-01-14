---
title:    "Javascript: Utskrift av felavhjälpande utdata"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

# Varför?

 Att skriva kod är en process som kan vara både rolig och utmanande. Men medan du skapar dina fantastiska appar eller webbsidor, är det viktigt att också ha kontroll över din kod och förstå vad som händer bakom kulisserna. Det är här utskrift av debug-information kommer in. Det är ett verktyg som kan hjälpa dig att felsöka din kod och hitta eventuella fel eller buggar som kan påverka din slutprodukt.

 # Hur man gör

 För att kunna skriva ut debug-information i din JavaScript-kod, behöver du använda en inbyggd funktion som heter `console.log()`. Detta är en funktion som skriver ut ett meddelande till din webbläsarkonsol. Detta kan vara mycket användbart när du vill se värdet av variabler eller resultatet av en funktion.

Här är ett enkelt exempel som visar hur du kan använda `console.log()` i din kod:

```Javascript
var num1 = 10;
var num2 = 20;
var summa = num1 + num2;

console.log("Summan av " + num1 + " och " + num2 + " är " + summa);
```

 I detta exempel skapar vi två variabler, `num1` och `num2`, och sedan lägger ihop dem och tilldelar resultatet till `summa`. Genom att använda `console.log()` kan vi se värdet av `summa` i vår konsol, vilket i detta fall ska vara 30.

En annan användbar funktion för att skriva ut debug-information är `console.table()`. Denna funktion listar ut värdena i en array eller ett objekt i en tabellform. Detta kan vara särskilt användbart när du arbetar med stora datamängder och vill ha en översiktlig vy.

# Djupdykning

Att skriva ut debug-information är ett viktigt verktyg för felsökning, men det är också viktigt att veta när och hur man ska använda det. Att överbelasta din kod med för många `console.log()`-utskrifter kan fördröja din programmets exekvering och göra det svårt att läsa och förstå koden. Det är därför viktigt att använda det med måtta och att ta bort dem när de inte längre behövs.

En annan sak att tänka på är att undvika att inkludera känslig information i dina utskrifter. Om du ska dela din kod med andra, se till att inte inkludera några lösenord eller andra känsliga uppgifter som kan vara skadliga om de hamnar i fel händer.

# Se också

- Lär dig mer om inbyggda funktioner i JavaScript på Mozilla Developers Network: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects
- Utforska olika metoder för att felsöka din JavaScript-kod på W3Schools: https://www.w3schools.com/js/js_debugging.asp
- Upptäck olika tillägg och verktyg som kan hjälpa dig med att felsöka din kod: https://medium.com/dailyjs/10-best-chrome-extensions-for-javascript-developers-41d554d2e5b4

# Se också

- Läs mer om inbyggda funktioner i JavaScript på Mozilla Developers Network: https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects
- Utforska olika metoder för att felsöka din JavaScript-kod på W3Schools: https://www.w3schools.com/js/js_debugging.asp
- Upptäck olika tillägg och verktyg som kan hjälpa dig med felsökning av din kod: https://medium.com/dailyjs/10-best-chrome-extensions-for-javascript-developers-41d554d2e5b4