---
title:                "Elixir: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum?

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine nützliche Technik in der Programmierung, da es uns ermöglicht, bestimmte Teile von Strings zu entfernen, die wir nicht benötigen. Dies kann hilfreich sein, wenn wir beispielsweise unerwünschte Zeichen aus Benutzereingaben entfernen oder Texte formatieren möchten.

## Wie geht man vor?

Um Zeichen basierend auf einem Muster zu löschen, können wir die `String.replace/3` Funktion in Elixir verwenden. Diese Funktion nimmt einen String, ein reguläres Ausdrucksmuster und eine Ersatzzeichenkette als Argumente entgegen.

Ein Beispiel könnte so aussehen:

```Elixir
original_string = "H3ll0 W0rld!" 
modified_string = String.replace(original_string, ~r/[0-9]/, "")
IO.puts modified_string # Output: Hll Wrld!
```

In diesem Beispiel haben wir alle Zahlen im ursprünglichen String durch eine leere Zeichenkette ersetzt, was zur Folge hatte, dass diese Zeichen im modifizierten String nicht mehr vorhanden sind.

Es ist auch möglich, den regulären Ausdruck im zweiten Argument durch eine Liste von Zeichen zu ersetzen, die gelöscht werden sollen. Zum Beispiel würden `String.replace(original_string, [?" "] , "")` alle Leerzeichen aus dem String entfernen.

## Tiefergehende Informationen

Es ist wichtig zu beachten, dass `String.replace/3` eine neue Kopie des ursprünglichen Strings zurückgibt und den ursprünglichen String nicht ändert. Wenn wir also sicherstellen wollen, dass der ursprüngliche String verändert wird, können wir die `String.replace!/3` Funktion verwenden, die den String direkt verändert.

Außerdem bietet Elixir viele weitere Funktionen zum Bearbeiten von Strings, wie zum Beispiel `String.split`, `String.trim`, `String.upcase` und viele mehr. Es lohnt sich, sich mit diesen Funktionen vertraut zu machen, um die Textmanipulation in Elixir optimal nutzen zu können.

## Siehe auch

- [Elixir String-Modul Dokumentation](https://hexdocs.pm/elixir/String.html)
- [Reguläre Ausdrücke in Elixir](https://elixirschool.com/de/lessons/advanced/regex/)
- [Einführung in die Textmanipulation mit Elixir](https://medium.com/@bnabach/elixir-text-manipulation-bcd68798b2e7)