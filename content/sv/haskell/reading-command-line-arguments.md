---
title:    "Haskell: Läsning av kommandoradsargument"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa in kommandoradsargument är en viktig del av att förstå hur man kan interagera med ett Haskell-program från terminalen. Genom att lära sig detta kan man skapa programs som är mer flexibla och anpassningsbara.

## Hur man gör det

Först och främst behöver man importera `System.Environment` biblioteket för att kunna använda dess funktioner för att läsa in kommandoradsargument. Här är ett exempel på en funktion som tar emot en lista av strängar (kommandoradsargumenten) och sedan returnerar en sträng med endast det första argumentet:

```haskell
import System.Environment

main = do
    args <- getArgs
    let firstArg = head args
    putStrLn ("Första argumentet är: " ++ firstArg)
```

Om man nu kör detta program från terminalen med `runhaskell namn_av_fil.hs första andra tredje` så kommer outputen att bli:

```bash
Första argumentet är: första
```

## Djupdykning

För de som är intresserade av lite mer avancerade sätt att läsa in kommandoradsargument finns det också andra funktioner tillgängliga i `System.Environment` som kan vara användbara. Till exempel finns det `getProgName` för att få programmets namn, `getEnvironment` för att få en lista av miljövariabler, och `lookupEnv` för att leta efter en specifik miljövariabel.

För att lära dig mer om dessa och andra funktioner, se dokumentationen för `System.Environment` biblioteket.

## Se även

- [System.Environment dokumentation](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html)
- [En handledning för att arbeta med Haskell i terminalen](https://www.fpcomplete.com/blog/2017/07/the-power-of-haskell-in-the-shell/)