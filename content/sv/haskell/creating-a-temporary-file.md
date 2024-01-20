---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att skapa en temporär fil innebär att man skapar en fil som endast används temporärt, vanligtvis för att lagra data tillfällig under en programs exekvering. Utvecklare gör detta när de behöver hantera stora mängder data som inte passar i RAM-minne eller när man behöver dela data mellan olika processer.

## Hur gör man:

Här är ett exempel på hur man skapar och skriver till en temporär fil i Haskell:

```Haskell
import System.IO.Temp (writeSystemTempFile)

main :: IO ()
main = do
    filepath <- writeSystemTempFile "temp.txt" "Här är lite data"
    putStrLn $ "Temporär fil skapad på: " ++ filepath
```
Kör programmet och du kommer se något liknande detta:

```Shell
Temporär fil skapad på: /tmp/temp.txt1234
```

## Djupdykning

1. Historisk bakgrund: Temporära filer har sitt ursprung i de tidiga dagarna av operativsystem, där datorerna var tvungna att dela resurser mellan många användare och processer. De gjorde det möjligt för systemen att lagra data temporärt utan att använda dyrbar RAM-minne.

2. Alternativ: Ett alternativ till temporära filer är att använda datatyper som listor eller bytearrays för att lagra data direkt i programminnet. Men dessa metoder har sina begränsningar och kan leda till minnesproblem om datan är för stor.

3. Implementeringsdetaljer: I Haskell används biblioteket `System.IO.Temp` för att skapa temporära filer. Funktionen `writeSystemTempFile` tar emot två argument: filnamnet och datan som ska skrivas. Den skapar en ny temporär fil och returnerar sökvägen till den.

## Se även

- Haskell dokumentation för biblioteket `System.IO.Temp`: http://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html
- En artikel om att hantera stora mängder data i Haskell: https://wiki.haskell.org/Dealing_with_large_data_sizes