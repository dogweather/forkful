---
title:                "Löschen von Zeichen mit einem Muster übereinstimmen"
html_title:           "Elm: Löschen von Zeichen mit einem Muster übereinstimmen"
simple_title:         "Löschen von Zeichen mit einem Muster übereinstimmen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist ein häufiges Problem für Programmierer. Es bezieht sich auf das Entfernen von Zeichen aus einer Zeichenkette, die einem bestimmten Muster entsprechen. Das kann hilfreich sein, wenn man beispielsweise bestimmte Symbole oder Zeichen aus einer Textdatei entfernen möchte.

Programmierer nutzen das Löschen von Zeichen, die einem Muster entsprechen, um Daten zu bereinigen oder Texte für die Weiterverarbeitung vorzubereiten. In manchen Fällen kann es auch verwendet werden, um Sicherheitslücken in Programmen zu schließen.

## Wie geht's?

Das Löschen von Zeichen, die einem Muster entsprechen, wird in Elm durch die Funktion `String.filter` ermöglicht. Diese Funktion nimmt zwei Argumente entgegen: ein Prädikat und eine Zeichenkette. Das Prädikat gibt an, welche Zeichen entfernt werden sollen, während die Zeichenkette die zu bereinigenden Daten enthält.

Ein Beispiel:

```Elm
import String

String.filter (\char -> char /= 'a') "example string"