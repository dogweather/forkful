---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "Gleam: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Löschen von Zeichen anhand bestimmter Muster ist eine häufige Aufgabe beim Programmieren. Es ermöglicht uns, unerwünschte Zeichen oder Textabschnitte aus unseren Daten zu entfernen und die gewünschten Informationen zu behalten.

Oftmals müssen Programmierer diese Aufgabe bewältigen, um Daten zu bereinigen oder um Code effizienter oder sicherer zu machen.

## Wie geht's?

Um Zeichen anhand eines bestimmten Musters zu löschen, gibt es in Gleam die Funktion `remove`. Hier ein einfaches Beispiel:

```Gleam
remove("a", "abracadabra") // Ergebnis: "brcdbr"
```

Die Funktion `remove` nimmt zwei Argumente entgegen: das zu löschende Muster und den Text, aus dem das Muster entfernt werden soll. Dabei wird jedes Vorkommen des Musters im Text gelöscht und der bereinigte Text zurückgegeben.

Auch die Kombination von mehreren Mustern ist möglich, wie in diesem Beispiel:

```Gleam
remove("a,v", "hello, world!") // Ergebnis: "hello world!"
```

Hier werden sowohl das Komma als auch der Buchstabe "a" gelöscht.

## Gründlicher Blick

Die `remove` Funktion ist nur eine der vielen Möglichkeiten, Zeichen in Gleam zu löschen. Es gibt auch andere Funktionen wie z.B. `replace`, die es uns erlauben, Zeichen anzupassen oder zu ersetzen.

Außerdem gibt es auch weitere Techniken, um unerwünschte Zeichen oder Textabschnitte zu entfernen. Dazu gehören z.B. reguläre Ausdrücke oder das Teilen von Strings in Substrings und das Herausfiltern bestimmter Teile.

In Gleam wird das Löschen von Zeichen durch die Standardbibliothek `gleam/strings` unterstützt. Hier finden sich viele nützliche Funktionen, die uns bei der Bearbeitung von Strings helfen.

## Siehe auch

- Offizielle Gleam Dokumentation: https://gleam.run/
- "Gleam Strings" in der Dokumentation: https://gleam.run/articles/strings.html