---
title:                "Suchen und Ersetzen von Text"
aliases:
- de/haskell/searching-and-replacing-text.md
date:                  2024-01-20T17:57:47.652017-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suchen und Ersetzen von Text"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Text suchen und ersetzen ermöglicht es, bestimmte Zeichenketten in einem Text zu finden und durch andere zu ersetzen. Programmierer tun das oft, um Fehler zu korrigieren, Daten zu aktualisieren oder Code refactoring zu betreiben.

## How to:
```Haskell
import Data.Text as T

-- Text suchen und ersetzen
replaceText :: T.Text -> T.Text -> T.Text -> T.Text
replaceText search replace source = T.replace search replace source

main :: IO ()
main = do
  let sourceText = "Hallo Welt! Hallo Programmierung!"
      searchText = "Hallo"
      replaceTextWith = "Guten Tag"
      newText = replaceText searchText replaceTextWith sourceText
  putStrLn $ T.unpack newText
```
Ausgabe:
```
Guten Tag Welt! Guten Tag Programmierung!
```

## Deep Dive
Textersetzungsoperationen sind so alt wie das Programmieren selbst. Ursprünglich hatten Editoren wie `sed` und `awk` bereits solche Funktionen in UNIX-Systemen. In Haskell wird diese Funktionalität durch Packages wie `text` ermöglicht, das effizient mit Text umgeht, im Gegensatz zu einfachen Strings.

Alternativen zu `text` sind Pakete wie `ByteString` für binäre Daten oder `regex` für komplexere Suchmuster. Die Implementation in Haskell ist oft funktional und immutabel, was diese Operationen sicher und voraussagbar macht.

## See Also
- [Haskell Text Package](https://hackage.haskell.org/package/text)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Hoogle – Haskell API search engine](https://hoogle.haskell.org/)
