---
title:                "Elm: Verknüpfung von Zeichenketten"
simple_title:         "Verknüpfung von Zeichenketten"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit der Zusammenführung von Zeichenfolgen beschäftigen? Nun, das Konkatenieren von Strings ist ein grundlegender Bestandteil der Programmierung, insbesondere in der Elm-Sprache. Es ermöglicht uns, verschiedene Texte zu einem zusammenzuführen und so vollständige Sätze oder Nachrichten zu erstellen. Ohne diese Funktion wäre es schwierig, dynamische Inhalte in unseren Programmen zu erstellen.

## How To

Um Strings in Elm zu konkatenieren, verwenden wir den Operator `++` zwischen zwei Strings. Hier ist ein Beispiel:

```Elm
"Mein Name ist" ++ "Jane" -- Ergebnis: "Mein Name ist Jane"
```
Wir können auch Variablen verwenden, anstatt direkt Strings zu konkatenieren:

```Elm
name = "Jane"
alter = 25
"Nur " ++ name ++ " ist schon " ++ (String.fromInt alter) ++ " Jahre alt." -- Ergebnis: "Nur Jane ist schon 25 Jahre alt."
```

Wie Sie sehen, können wir auch andere Datentypen wie Integer mithilfe von `String.fromInt` in Strings konvertieren und sie dann konkatenieren.

## Deep Dive

Die Konkatenierung von Strings in Elm ist dank der rein funktionalen Natur der Sprache sehr einfach und robust. Im Gegensatz zu anderen Sprachen, in denen Strings als mutable (veränderbar) betrachtet werden, sind in Elm alle Strings unveränderbar. Das bedeutet, dass jedes Mal, wenn wir Strings konkatenieren, ein komplett neuer String erstellt wird, anstatt den ursprünglichen String zu ändern. Dadurch vermeiden wir unerwartete Seiteneffekte und unser Code wird insgesamt zuverlässiger.

Ein weiterer Vorteil der funktionalen Natur von Elm ist, dass wir die Konkatenierung von Strings auf elegante Weise in Funktionen verwenden können. Zum Beispiel können wir eine Funktion erstellen, die zwei Strings konkateniert und ein Satzzeichen am Ende hinzufügt:

```Elm
concatenateStrings str1 str2 =
    str1 ++ " " ++ str2 ++ "."

concatenateStrings "Hallo" "Welt" -- Ergebnis: "Hallo Welt."
```

Wie Sie sehen, können wir diese Funktion verwenden, um eine Vielzahl von Strings zu konkatenieren, indem wir die Parameter ändern. Dadurch wird unser Code wiederverwendbarer und einfacher zu warten.

## Siehe auch

Für weitere Informationen über die Konkatenierung von Strings in Elm und andere grundlegende Sprachkonzepte empfehle ich Ihnen diese Ressourcen:

- [Offizielle Elm-Website](https://elm-lang.org/)
- [Elm-Dokumentation](https://package.elm-lang.org/)
- [Elm-Tutorial auf Deutsch](https://www.elm-tutorial.org/de/)