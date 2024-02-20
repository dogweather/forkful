---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:25.262073-07:00
description: "Att skriva till standardfel (stderr) i Haskell g\xF6r det m\xF6jligt\
  \ f\xF6r program att skilja sin utdata mellan normala resultat och felmeddelanden.\
  \ Detta \xE4r\u2026"
lastmod: 2024-02-19 22:04:57.192609
model: gpt-4-0125-preview
summary: "Att skriva till standardfel (stderr) i Haskell g\xF6r det m\xF6jligt f\xF6\
  r program att skilja sin utdata mellan normala resultat och felmeddelanden. Detta\
  \ \xE4r\u2026"
title: Skriva till standardfel
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till standardfel (stderr) i Haskell gör det möjligt för program att skilja sin utdata mellan normala resultat och felmeddelanden. Detta är avgörande för att signalera problem och felsöka, utan att förväxla den standardutdata (stdout) som ofta bär programmets huvuddata eller resultat.

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
