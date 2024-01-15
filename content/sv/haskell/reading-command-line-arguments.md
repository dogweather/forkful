---
title:                "Läsning av kommandoradsargument"
html_title:           "Haskell: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Varför

Att lära sig läsa kommandoradsargument i Haskell kan vara användbart för att skapa program som kan interagera med användaren via terminalen. Det kan också göra det enklare att testa och felsöka program genom att kunna ge olika argument vid körning.

# Hur man läser kommandoradsargument i Haskell

För att läsa kommandoradsargument i Haskell använder man funktionen `getArgs` från standardbiblioteket `System.Environment`. Denna funktion returnerar en lista av strängar som motsvarar de argument som skickats med vid körning av programmet.

Ett enkelt exempel på användning av `getArgs` är att skapa ett program som tar emot två argument och gör en beräkning med dem:

```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
    [arg1, arg2] <- getArgs
    let result = read arg1 + read arg2 :: Int
    putStrLn ("Summan av " ++ arg1 ++ " och " ++ arg2 ++ " är " ++ show result)
```

Om man sedan kör programmet i terminalen med kommandot `runhaskell program.hs 5 8`, kommer följande att skrivas ut:

`Summan av 5 och 8 är 13`

Notera att `getArgs` returnerar en lista av strängar, så vi måste använda `read` för att konvertera till rätt datatyp (i detta fall `Int`) för att kunna utföra beräkningen.

# Djupdykning

För att läsa kommandoradsargument mer avancerat och exakt kan man använda sig av till exempel paketet [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative). Med detta kan man definiera egna flaggor och argument med detaljerad information om parametrar och typ av värde som förväntas.

Man kan också använda sig av funktioner som `lookupEnv` från `System.Environment` för att läsa miljövariabler eller `getProgName` för att få namnet på det körande programmet.

# Se även

- [Haskell Command Line Arguments](https://wiki.haskell.org/Command_line_arguments)
- [System.Environment documentation](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Environment.html)