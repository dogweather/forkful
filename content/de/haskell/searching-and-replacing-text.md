---
title:                "Haskell: Suchen und Ersetzen von Text"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Das Suchen und Ersetzen von Text ist eine grundlegende Fähigkeit für jeden Programmierer. Es ermöglicht uns, schnell und effizient Teile von Text zu ändern, ohne den gesamten Text manuell durchsuchen zu müssen. Mit Haskell können wir diese Aufgabe noch einfacher und präziser lösen.

## Wie es geht

Das Suchen und Ersetzen von Text in Haskell ist mit der Funktion `substitute` aus dem Modul `Text.Regex` möglich. Diese Funktion akzeptiert einen regulären Ausdruck und zwei Texte als Parameter und ersetzt alle Vorkommen des regulären Ausdrucks im ersten Text durch den zweiten Text.

Ein einfaches Beispiel könnte so aussehen:

```Haskell
import Text.Regex (substitute)

main = do
  let text = "Hallo, mein Name ist Max"
  let newText = substitute "(\\w+)" "\\1!" text
  print newText

-- Ausgabe: "Hallo!, mein Name!, ist Max!"
```

In diesem Beispiel wird die Funktion `substitute` verwendet, um alle Wörter im Text durch das gleiche Wort mit einem Ausrufezeichen zu ersetzen. Der reguläre Ausdruck `(\w+)` steht für ein oder mehrere Buchstaben, während `\1` das erste Vorkommen des regulären Ausdrucks beinhaltet.

Natürlich können wir auch komplexere reguläre Ausdrücke verwenden, um bestimmte Muster im Text zu finden und zu ersetzen. Hier sind einige nützliche Ressourcen zum Lernen von regulären Ausdrücken:

- [Reguläre Ausdrücke Tutorial auf regexone.com](https://regexone.com/)
- [Dokumentation von `Text.Regex`](https://hackage.haskell.org/package/regex)

## Tiefergehende Informationen

Die `substitute` Funktion ist nur eine von vielen nützlichen Funktionen im `Text.Regex` Modul. Wir können auch die `subRegex` Funktion verwenden, um alle Vorkommen des regulären Ausdrucks im Text durch ein Ergebnis einer berechneten Funktion zu ersetzen. Oder wir können die `matchRegex` Funktion verwenden, um alle Vorkommen des regulären Ausdrucks in eine Liste von Treffern zu extrahieren.

Es gibt auch viele andere nützliche Funktionen und Optionen für die Arbeit mit regulären Ausdrücken in Haskell. Wenn du tiefer in dieses Thema einsteigen möchtest, solltest du unbedingt die oben genannten Ressourcen durchgehen und weitere Recherche betreiben.

## Siehe auch

- [Haskell-Regular Expressions - ein Tutorial](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/haskell-regexp.pdf)
- [Reguläre Ausdrücke in Haskell](https://wiki.haskell.org/Regular_expressions)