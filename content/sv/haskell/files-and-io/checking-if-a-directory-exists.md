---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:27.103418-07:00
description: "Hur man g\xF6r: Haskell erbjuder, via sitt basbibliotek, raka v\xE4\
  gar att kontrollera om en katalog existerar, fr\xE4mst genom att anv\xE4nda modulen\u2026"
lastmod: '2024-03-13T22:44:37.969164-06:00'
model: gpt-4-0125-preview
summary: "Haskell erbjuder, via sitt basbibliotek, raka v\xE4gar att kontrollera om\
  \ en katalog existerar, fr\xE4mst genom att anv\xE4nda modulen `System.Directory`."
title: Kontrollera om en katalog existerar
weight: 20
---

## Hur man gör:
Haskell erbjuder, via sitt basbibliotek, raka vägar att kontrollera om en katalog existerar, främst genom att använda modulen `System.Directory`. Låt oss titta på ett grundläggande exempel:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/path/to/your/directory"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "Finns katalogen? " ++ show exists
```

Exempel på utdata, beroende på om katalogen finns:

```
Finns katalogen? True
```
Eller:
```
Finns katalogen? False
```

För mer komplexa scenarier eller ytterligare funktionalitet kan du överväga ett populärt tredjepartsbibliotek som `filepath` för att hantera och manipulera filvägar på ett mer abstrakt sätt. Dock, för syftet att enbart kontrollera om en katalog finns, är basbibliotekets `System.Directory` tillräckligt och effektivt.

Kom ihåg att arbete med filsystem kan variera över plattformar, och Haskeells tillvägagångssätt syftar till att abstrahera bort en del av dessa skillnader. Testa alltid dina filoperationer på målsystemet för att säkerställa förväntat beteende.
