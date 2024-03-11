---
date: 2024-01-26 03:44:03.220410-07:00
description: "Das Runden von Zahlen ist das Anpassen einer Dezimalzahl auf ihren n\xE4\
  chsten ganzzahligen Wert oder auf eine bestimmte Anzahl von Nachkommastellen.\u2026"
lastmod: '2024-03-11T00:14:27.692540-06:00'
model: gpt-4-0125-preview
summary: "Das Runden von Zahlen ist das Anpassen einer Dezimalzahl auf ihren n\xE4\
  chsten ganzzahligen Wert oder auf eine bestimmte Anzahl von Nachkommastellen.\u2026"
title: Zahlen runden
---

{{< edit_this_page >}}

## Was & Warum?

Das Runden von Zahlen ist das Anpassen einer Dezimalzahl auf ihren nächsten ganzzahligen Wert oder auf eine bestimmte Anzahl von Nachkommastellen. Programmierer runden, um die Komplexität zu reduzieren, die Lesbarkeit zu verbessern oder Präzisionsanforderungen zu erfüllen.

## Wie geht das:

Das `Basics`-Modul von Elm bietet praktische Funktionen zum Runden: `round`, `floor` und `ceiling`. Hier ist, wie man sie verwendet.

```elm
import Basics exposing (round, floor, ceiling)

-- Auf die nächste ganze Zahl runden
round 3.14    --> 3
round 3.5     --> 4

-- Abrunden
floor 3.999   --> 3

-- Aufrunden
ceiling 3.001 --> 4

-- Dezimalstellen abschneiden ohne zu runden
truncate 3.76 --> 3
```

Elm bietet auch `toLocaleString` an, um auf eine feste Anzahl von Dezimalstellen zu runden:

```elm
import Float exposing (toLocaleString)

-- Auf zwei Dezimalstellen runden
toLocaleString 2 3.14159 --> "3.14"
```

## Vertiefung

Elm ist eine streng typisierte funktionale Sprache, die Nebenwirkungen an die "Ränder" der Architektur delegiert. Das bedeutet, dass Funktionen wie das Runden rein und vorhersehbar sein müssen. Historisch gesehen ist das Runden eine gängige Operation in vielen Programmiersprachen, die sich mit der Ungenauigkeit der Fließkommazahlen-Arithmetik befassen.

Elms Ansatz zum Runden ist unkompliziert - die Funktionen sind rein und halten sich an mathematische Definitionen für round, floor und ceiling. Elm antizipiert die gängigen Bedürfnisse, indem es eingebaute Funktionen zur Verfügung stellt, da das Präzisionsmanagement eine häufige Anforderung ist, besonders in den Bereichen Finanzen und Grafik.

Alternativen zu den eingebauten Funktionen von Elm könnten benutzerdefinierte Implementierungen unter Verwendung von arithmetischen Operationen umfassen, aber das fügt unnötige Komplexität hinzu, wenn die Standardbibliothek die Aufgabe bereits effizient erfüllt.

In der aktuellen Version verwendet Elm die zugrundeliegende Fließkommazahlen-Mathematik von JavaScript für diese Operationen und bleibt damit konsistent mit dem IEEE 754-Standard, was bei der Berücksichtigung von Präzision und potenziellen Fließkommazahlen-Fehlern zu bedenken ist.

## Siehe auch

- Offizielle Dokumentation des `Basics`-Moduls von Elm: https://package.elm-lang.org/packages/elm/core/latest/Basics
- Ein detaillierter Blick darauf, wie Fließkommazahlen in der Informatik funktionieren: https://floating-point-gui.de/
- Elm `Float`-Modul für weitere Fließkommazahlen-Operationen: https://package.elm-lang.org/packages/elm/core/latest/Float
