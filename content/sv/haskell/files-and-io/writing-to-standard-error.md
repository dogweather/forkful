---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:25.262073-07:00
description: "Hur man g\xF6r: I Haskell \xE4r det enkelt att skriva till stderr med\
  \ basbibliotekets `System.IO`-modul. Nedan f\xF6ljer ett grundl\xE4ggande exempel\
  \ f\xF6r att\u2026"
lastmod: '2024-03-13T22:44:37.971206-06:00'
model: gpt-4-0125-preview
summary: "I Haskell \xE4r det enkelt att skriva till stderr med basbibliotekets `System.IO`-modul."
title: Skriva till standardfel
weight: 25
---

## Hur man gör:
I Haskell är det enkelt att skriva till stderr med basbibliotekets `System.IO`-modul. Nedan följer ett grundläggande exempel för att demonstrera:

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Detta är ett felmeddelande."
```

Utdata från detta program till stderr skulle vara:

```
Detta är ett felmeddelande.
```

Om du arbetar med en mer komplex applikation, eller om du behöver bättre kontroll över loggning (inklusive fel), kan du överväga att använda ett tredjepartsbibliotek. Ett populärt alternativ är `monad-logger` som integreras med Haskell-programmeringsstilen `mtl`. Här är ett litet kodsnutt som använder `monad-logger`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "Detta är ett felmeddelande som använder monad-logger."
```

När den körs, ger `monad-logger`-versionen liknande ut en felmeddelande, men den är utrustad med mer sammanhang som tidsstämplar eller loggnivåer, beroende på konfigurationen:

```
[Error] Detta är ett felmeddelande som använder monad-logger.
```

Båda metoderna tjänar syftet att skriva till stderr, med valet som i stor utsträckning beror på din applikations komplexitet och behov.
