---
title:                "Entfernen von Zeichen mit passendem Muster"
html_title:           "Elixir: Entfernen von Zeichen mit passendem Muster"
simple_title:         "Entfernen von Zeichen mit passendem Muster"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Was ist das Löschen von Zeichen passend zu einem Muster und warum machen Programmierer das?

Das Löschen von Zeichen passend zu einem Muster ist ein gängiges Konzept in der Programmierung, bei dem ein Programm Teile eines Text- oder Zeichenstrings entfernt, die einem bestimmten Muster entsprechen. Dies kann nützlich sein, um unerwünschte Zeichen oder Wörter aus einem Text zu entfernen oder um eine bestimmte Formatierung aufrechtzuerhalten. Programmierer nutzen diese Funktion, um Texte zu manipulieren und Daten zu filtern oder zu bereinigen.

Wie geht es weiter?

In Elixir gibt es verschiedene Möglichkeiten, um Zeichen passend zu einem Muster zu löschen. Eine Möglichkeit ist die Verwendung der String-Modul-Funktion "replace". Hier ist ein Beispiel, um alle Vokale aus einem String zu entfernen:

```Elixir 
  str = "Hallo Welt"
  new_str = String.replace(str, ~r/[aeiou]/, "")
  IO.puts new_str
```
Das Ergebnis wäre "Hll Wlt".

Eine andere Option ist die Verwendung von regulären Ausdrücken in Kombination mit der in Elixir integrierten Funktion "String.replace". Hier ist ein Beispiel, um alle Sonderzeichen aus einem String zu entfernen:

```Elixir 
  str = "H@llo W#lt"
  new_str = String.replace(str, ~r/[^[:alnum:][:space:]]/, "")
  IO.puts new_str
```
Das Ergebnis wäre "Hallo Welt".

Tiefere Einblicke

Das Konzept des Löschens von Zeichen passend zu einem Muster hat seinen Ursprung in der String-Manipulationstechnik. Es wurde erstmals in UNIX-Systemen verwendet und hat sich seitdem zu einem Standard-Workflow in der Programmierung entwickelt. In Elixir gibt es auch andere Möglichkeiten, um Zeichen passend zu einem Muster zu löschen, wie z.B. die Funktion "clean" im Modul "String". Es ist wichtig zu beachten, dass das Löschen von Zeichen passend zu einem Muster nicht nur für Texte, sondern auch für andere Datentypen wie Listen und Maps verwendet werden kann.

Weitere Quellen

Elixir-Dokumentation: https://hexdocs.pm/elixir/String.html#replace-expect:2
Reguläre Ausdrücke in Elixir: https://elixir-lang.org/getting-started/regex.html
Erweiterte String-Manipulation in Elixir: https://devhints.io/elixir-string