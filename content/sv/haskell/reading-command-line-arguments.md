---
title:                "Haskell: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa in kommandoradsargument kan vara en enormt användbar färdighet för Haskell-programmerare. Genom att läsa argument som har skickats till ett program vid körning kan man anpassa programmet baserad på användarens önskemål och behov. Detta kan öka användbarheten och användarupplevelsen av ett program avsevärt.

## Hur man gör

För att läsa in kommandoradsargument måste man först importera "System.Environment" biblioteket. Sedan kan man använda funktionen "getArgs" för att hämta en lista med argumenten som skickats vid körning.

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn $ "Du angav följande argument: " ++ show args
```

Om man kör programmet med kommandot "runhaskell program.hs arg1 arg2" kommer man att få utskriften "Du angav följande argument: [\"arg1\",\"arg2\"]".

Man kan sedan använda listan av argument för att anpassa programmet på olika sätt, till exempel genom att köra specifika funktioner beroende på vilka argument som angetts.

## Djupdykning

Förutom att bara läsa in argumenten som skickas vid körning finns det flera andra funktioner som kan användas för att hantera kommandoradsargument i Haskell.

En användbar funktion är "getProgName", som hämtar namnet på det körda programmet. Detta kan vara användbart för att skapa program som fungerar på flera olika platformar, där programnamnet kan variera.

Man kan också använda funktionen "lookupEnv" för att hämta värden på miljövariabler som skickats till programmet vid körning.

Det finns också bibliotek som "optparse-applicative" som förenklar hanteringen av kommandoradsargument och låter användaren definiera vilka argument som ska stödjas och vilken funktionalitet de ska ha.

## Se även

* [Haskell Dokumentation - System.Environment](https://downloads.haskell.org/~ghc/6.12.2/docs/html/libraries/base-4.2.0.1/System-Environment.html)
* [Real World Haskell - Command-Line Option Parsing ](https://www.realworldhaskell.org/blog/2019/07/07/commandline-options-parsing) 
* [Optparse-applicative på Hackage](https://hackage.haskell.org/package/optparse-applicative)