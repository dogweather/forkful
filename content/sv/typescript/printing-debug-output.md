---
title:    "TypeScript: Utskrift av felloggning"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

När vi skriver kod, är vår huvuduppgift att se till att den fungerar korrekt. Men ibland, när ett problem uppstår, är det svårt att veta var det gick snett. Då kan det vara till nytta att använda sig av debug-utskrifter. Genom att skriva ut data i olika delar av koden kan vi få en bättre förståelse för vad som händer och eventuella fel som uppstår.

## Hur man gör

 För att skriva ut data i TypeScript kan vi använda oss av "console.log()" funktionen. Låt oss säga att vi har en variabel "namn" som innehåller en sträng, och vi vill skriva ut den till konsolen. Vi skulle då kunna använda koden nedan:

 ```TypeScript
 let namn = "Erika";
 console.log(namn);
 ```

 Detta kommer att skriva ut strängen "Erika" till konsolen när vi kör vår kod. Vi kan också skriva ut flera variabler eller till och med kombinera text och variabler. Exempelvis:

 ```TypeScript
 let år = 2021;
 console.log("Hej " + namn + ", det är nu år " + år + "!");
 ```

 Detta kommer att skriva ut "Hej Erika, det är nu år 2021!" i konsolen. Genom att använda oss av debug-utskrifter på detta sätt kan vi se värdet på våra variabler och kontrollera om de håller våra förväntade värden.

## Djupdykning

Det finns flera andra sätt att skriva ut data i TypeScript, såsom "console.info()", "console.warn()" och "console.error()". Dessa funktioner ger oss möjlighet att skriva ut olika typer av meddelanden och varningar till konsolen. Det är även möjligt att använda "console.table()" för att skriva ut data som en tabell istället för en rad med text.

Det finns också olika sätt att manipulera utskrifterna, såsom att formatera numeriska värden eller använda färger för att tydliggöra olika typer av meddelanden. Genom att experimentera och lära sig olika sätt att använda debug-utskrifter kan vi få en bättre förståelse för vår kod och felsöka mer effektivt.

## Se även

Här är några andra artiklar som kan vara intressanta för dig:

- [10 Tips för Felsökning i TypeScript](https://www.linkedin.com/pulse/10-fels%C3%B6kning-i-typescript-tony-letey/)
- [Aningens djupare om console.log i TypeScript](https://www.morain.se/know-how/blog_ser4.php)
- [Debugga din kod som ett proffs med TypeScript](https://medium.com/@charliejackson/debugging-your-code-like-a-pro-with-typescript-aa718279f02f)