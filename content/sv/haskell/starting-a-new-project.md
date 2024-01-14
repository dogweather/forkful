---
title:                "Haskell: Att påbörja ett nytt projekt"
programming_language: "Haskell"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför

Att starta ett nytt projekt i Haskell kan verka skrämmande för vissa, men det finns många goda skäl att ge sig in i denna spännande värld av funktionell programmering. Haskell erbjuder en robust och säker kodningserfarenhet med hjälp av sin strikta typning och starka statiska typsystem. Det finns också ett stort samhälle av Haskell-utvecklare som är redo att hjälpa till och stödja dig i ditt projekt.

## Hur man gör

För att starta ett Haskell-projekt behöver du först installera en lämplig Haskell-miljö på din dator. Det finns flera alternativ, men de två mest populära är ghc (Glasgow Haskell Compiler) och stack. När du har installerat den valda miljön kan du skapa ett nytt projekt med hjälp av enkla terminalkommandon:

```Haskell
stack new my-project
```

Detta kommer att skapa en grundläggande projektstruktur med en fil som heter Main.hs som innehåller en "Hello World" -funktion. Du kan sedan använda kommandot `stack init` för att konfigurera ditt projekt ytterligare och lägga till eventuella beroenden.

För att kompilera och köra ditt projekt, använd följande kommandon:

```Haskell
stack build
stack exec my-project
```

Du kan också använda en interaktiv interpreter, GHCi, för att testa ditt projekt i realtid. Använd kommandot `stack ghci` för att starta den.

## Djupdykning

När du har skapat ditt projekt kan vi nu titta på några viktiga aspekter av Haskell-programmering. En av de viktigaste egenskaperna i Haskell är dess strikta typsystem. Detta innebär att alla värden och funktioner har en specifik typ som måste följas. Typsystemet hjälper till att undvika buggar och gör det möjligt att känna till ditt programs beteende utan att ens köra det. Det kan verka krångligt till en början, men det kommer att hjälpa dig att skriva säkrare och mer robust kod på lång sikt.

En annan viktig del av Haskell är dess funktionella programmeringstil. Detta innebär att programmet består av funktioner och att värden och variabler är oföränderliga (immutable). Detta kan kännas annorlunda jämfört med andra programmeringsspråk, men det hjälper till att undvika buggar och gör det lättare att förstå hur koden fungerar.

Det finns också många olika verktyg och paket för att hjälpa till med utvecklingen i Haskell, såsom textredigerare, pakethanterare och testramar. Att lära sig om dessa verktyg och hur man använder dem effektivt kan göra en stor skillnad i ditt projekt.

Det finns såklart mycket mer att lära sig om Haskell och dess möjligheter. Men att börja med ett nytt projekt är en bra första steg för att få en bättre förståelse för språket.

## Se också

- Inledning till Haskell (https://www.haskell.org/)
- GHC-dokumentation (https://www.haskell.org/ghc/)
- Stack-dokumentation (https://docs.haskellstack.org/en/stable/README/)