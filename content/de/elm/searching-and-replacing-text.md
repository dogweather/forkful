---
title:                "Suchen und Ersetzen von Text"
html_title:           "Bash: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?

Suchen und Ersetzen von Text ist eine gängige Funktion in der Programmierung. Sie ermöglicht es, spezifische Zeichenketten in einem Text zu identifizieren und durch andere zu ersetzen. Programmierer verwenden sie oft, um große Datenmengen zu bereinigen oder zu transformieren.

## So geht's:

Unter Verwendung der `replace`-Funktion aus dem `String`-Modul in Elm können wir in Zeichenketten suchen und ersetzen. Hier ist ein einfaches Beispiel:

```Elm
import String

alterText = "Hallo Welt"
neuerText = String.replace "Welt" "Elm" alterText
```

Wenn Sie dieses Programm ausführen, würde es "Hallo Elm" ausgeben, da es "Welt" durch "Elm" ersetzt.

## Tiefere Einblicke:

Das Suchen und Ersetzen von Text hat eine lange Geschichte in der Informatik. Es gibt viele alternative Ansätze, etwa mit regulären Ausdrücken oder den Einsatz von CRDTs (Conflict-free Replicated Data Types) in verteilter Programmierung. In Elm verwendet die `replace`-Funktion einen effizienten String-Matching-Algorithmus, jedoch ohne Unterstützung für reguläre Ausdrücke.

## Siehe auch:

Zum Vertiefen der Inhalte eignen sich folgende Ressourcen:

1. Elm Dokumentation für das [`String`](https://package.elm-lang.org/packages/elm/core/latest/String) Modul.
2. Implementation des `replace`-Algorithmus in [Elm Core](https://github.com/elm/core/blob/1.0.2/src/Native/String.js#L143).
3. Ein Tutorial über [reguläre Ausdrücke](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/Regular_Expressions) auf MDN Web Docs.
4. Ein Artikel über die Nutzung von [CRDTs](https://josephg.com/blog/crdts-are-the-future/) in der Programmierung.