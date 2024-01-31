---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:57:01.593021-07:00
html_title:           "Fish Shell: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"

category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kolla om en mapp finns är processen att bekräfta mappens närvaro i filsystemet. Programmerare gör detta för att undvika fel vid försök att läsa från eller skriva till icke-existerande mappar.

## Så här gör du:
För att kontrollera om en mapp finns i Haskell kan du använda `doesDirectoryExist` funktionen från `System.Directory` modulen.

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let directoryPath = "/vägen/till/din/mapp"
  directoryExists <- doesDirectoryExist directoryPath
  putStrLn $ "Finns mappen? " ++ show directoryExists
```

Kör du koden ger det antingen `Finns mappen? True` om mappen finns, eller `Finns mappen? False` om den inte gör det.

## Djupdykning
Historiskt sett har filsystemshanteringen i många programmeringsspråk varit grundläggande. I Haskell tillhandahålls operativsystem-relaterade funktioner genom `System.Directory` modulen, som utvecklats över tid för att erbjuda ett högre abstraktionslager för fil- och katalogoperationer. Alternativ till `doesDirectoryExist` inkluderar att fånga undantag som kastas när man försöker accessa en icke-existerande mapp eller att använda tredjepartsbibliotek som ger mer sofistikerade verktyg för filsystemsinteraktion. Det är värt att notera att `doesDirectoryExist` utför ett IO-samtal och därför resultatet är inom `IO` monaden vilket reflekterar potentiellt föränderliga filsystemstillstånd.

## Se Också
- Haskell Documentation for `System.Directory`: https://hackage.haskell.org/package/directory
- Real World Haskell, Kapitel 9 – "Files and streams": http://book.realworldhaskell.org/read/io.html
- Haskell IO Tutorial: https://www.haskell.org/tutorial/io.html
