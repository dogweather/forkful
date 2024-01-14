---
title:                "Haskell: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Viele Programmiererinnen und Programmierer müssen Texte in ihrem Code ändern - sei es, um einen Tippfehler zu korrigieren oder um ein bestimmtes Wort oder eine Zeichenfolge durch eine andere zu ersetzen. In Haskell gibt es einige einfache und effektive Möglichkeiten, um ein solches Suchen und Ersetzen von Text durchzuführen.

## Wie geht's

Um Text in Haskell zu suchen und zu ersetzen, gibt es drei Hauptfunktionen: `words`, `unwords` und `replace`. Diese können in Kombination verwendet werden, um gewünschte Ergebnisse zu erzielen.

```Haskell
-- Beispiel-Liste von Wörtern
list = ["Hallo", "Welt", "Hallo", "Haskell"]

-- `words` wandelt einen String in eine Liste von Wörtern um
words "Hallo Welt" == ["Hallo", "Welt"]

-- `unwords` wiederum fügt eine Liste von Wörtern zu einem String zusammen
unwords list == "Hallo Welt Hallo Haskell"

-- `replace` ersetzt ein bestimmtes Wort oder eine Zeichenfolge in einem String durch eine andere
replace "Haskell" "Programming" "Hallo Welt Hallo Haskell" == "Hallo Welt Hallo Programming"
```

Ein Beispiel, um einen Tippfehler in einem String zu korrigieren, sieht folgendermaßen aus:

```Haskell
incorrect = "Guten Nur"

correct = replace "Nur" "Tag" incorrect

-- Output:
correct == "Guten Tag"
```

## Tief eintauchen

Die `replace` Funktion ist der Kern des Suchens und Ersetzens von Text in Haskell. Sie akzeptiert drei Argumente: Die zu ersetzende Zeichenfolge, die neue Zeichenfolge und der String, in dem dies geschehen soll. Diese Funktion kann auch verschachtelt werden, um mehrere Ersetzungen auf einmal vorzunehmen.

```Haskell
-- Beispiel einer verschachtelten `replace` Funktion
replace "Hallo" "Hi" . replace "Haskell" "Programming" $ "Hallo Welt Hallo Haskell"

-- Output:
"Hi Welt Hi Programming"
```

Es ist auch möglich, die `replace` Funktion mit regulären Ausdrücken aus dem `Text.Regex` Modul zu kombinieren, um komplexere Such- und Ersetzungsvorgänge durchzuführen.

## Siehe auch

- [Haskell-Dokumentation zu `replace`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:replace)
- [Tutorial zu regulären Ausdrücken in Haskell](https://wiki.haskell.org/Regular_expressions)
- [Beispiele für das Suchen und Ersetzen von Text in Haskell](https://www.codewars.com/kata/search-and-replace/train/haskell)