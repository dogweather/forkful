---
title:                "Suchen und Ersetzen von Text"
html_title:           "Elixir: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Suchen und Ersetzen von Text ist eine grundlegende Aufgabe in der Programmierung. Es bezieht sich auf das Durchsuchen eines Textes, um bestimmte Zeichenfolgen zu finden und durch andere Zeichenfolgen zu ersetzen. Programmierer tun dies, um Texte effektiv zu bearbeiten und zu manipulieren, einschließlich der Behebung von Fehlern oder dem Durchführen von automatisierten Aufgaben.

## Wie geht's?
```Elixir
## Beispiel 1
Ersetze "hello" durch "guten Tag" in einem String:
String.replace("Hello, world!", "hello", "guten Tag")
# Ausgabe: "Goodbye, world!"

## Beispiel 2
Ersetze alle Vokale in einem String durch "x":
String.replace("Hello, world!", ~r/[aeiou]/, "x")
# Ausgabe: "Hxlx, wxrld!"
```

## Tieferer Einblick
Die Aufgabe des Suchens und Ersetzens von Text ist nicht neu und wird auch in anderen Programmiersprachen durchgeführt. Elixir bietet jedoch eine robuste und effiziente Möglichkeit, dies zu tun, dank der eingebauten Funktion `String.replace/3`. Es gibt auch alternativen Ansätze wie reguläre Ausdrücke oder Bibliotheken wie `Regex`. Die Implementierung von `String.replace/3` nutzt die Leistungsfähigkeit des Elixir "Polyglotismus" und verwendet gleichzeitig die Leistungsfähigkeit der eingebauten Funktionen der Sprache.

## Siehe auch
- Offizielle Dokumentation zu `String.replace/3`: https://hexdocs.pm/elixir/String.html#replace/3
- Die Verwendung von regulären Ausdrücken in Elixir: https://elixirschool.com/de/lessons/advanced/regular-expressions/