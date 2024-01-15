---
title:                "Suchen und Ersetzen von Text."
html_title:           "Haskell: Suchen und Ersetzen von Text."
simple_title:         "Suchen und Ersetzen von Text."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Warum

Wenn du viel mit Texten arbeitest, weißt du wahrscheinlich, wie mühsam es sein kann, Text manuell zu suchen und zu ersetzen. Zum Glück gibt es in der aktuellen Version von Haskell viele effiziente Funktionen, die das Suchen und Ersetzen von Text erleichtern.

# Wie es geht

Um Text in Haskell zu suchen und zu ersetzen, gibt es einige nützliche Funktionen, die du verwenden kannst. Hier sind ein paar Beispiele:

```
import Data.Text

-- Suchen und ersetzen von Text
replace "Hallo" "Hi" "Hallo, wie geht es dir?" 
-- Output: "Hi, wie geht es dir?"

-- Großbuchstaben in Kleinbuchstaben umwandeln
toLower "Hallo WELT" 
-- Output: "hallo welt"

-- Extrahieren von Teilstrings
take 5 "Hallo Welt" 
-- Output: "Hallo"
```

# Tiefer Einblick

Haskell bietet viele Funktionen zum Suchen und Ersetzen von Text, einschließlich der Verwendung von regulären Ausdrücken mit der Bibliothek `regex`. Du kannst auch eigene Funktionen schreiben, die auf individuelle Anforderungen zugeschnitten sind, indem du die vielen integrierten Datentypen und Operatoren von Haskell kombinierst.

# Siehe auch

- Offizielle Haskell-Dokumentation
- Einführung in Haskell auf Deutsch
- Reguläre Ausdrücke in Haskell