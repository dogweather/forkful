---
title:                "String-Interpolation"
html_title:           "Fish Shell: String-Interpolation"
simple_title:         "String-Interpolation"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
String-Interpolation ist ein häufig verwendetes Konzept in der Programmierung, das es ermöglicht, dynamische Variablen oder Ausdrücke in eine Zeichenkette einzufügen, um diese besser lesbar und flexibler zu gestalten. Programmierer nutzen dies, um das Einfügen von Variablenwerten oder anderen Ausdrücken in Texten, beispielsweise in Fehlermeldungen oder Ausgabemeldungen, zu erleichtern.

## So geht's:
Fish Shell bietet eine einfache und intuitive Möglichkeit, String-Interpolation zu nutzen. Dazu werden die Variablen oder Ausdrücke in geschweifte Klammern innerhalb der Zeichenkette eingefügt. Bei der Ausführung des Skripts werden diese Klammern automatisch durch die entsprechenden Werte oder Ausdrücke ersetzt. Hier ein Beispiel:

```Fish Shell

set name "Max"
set age 25
echo "Mein Name ist {$name} und ich bin {$age} Jahre alt."
```
Ausgabe: "Mein Name ist Max und ich bin 25 Jahre alt."

## Tiefere Einblicke:
String-Interpolation gibt es schon seit vielen Jahren in verschiedenen Programmiersprachen. Auch in Fish Shell sind alternative Methoden wie die `eval`-Funktion bekannt, aber die Verwendung von geschweiften Klammern ist in der Regel die bevorzugte Methode, da sie übersichtlicher und sicherer ist.

Die Implementierung von String-Interpolation in Fish Shell basiert auf der Verwendung von Unterstreichen vor und nach Variablen oder Ausdrücken innerhalb von Zeichenketten. Diese Unterstriche signalisieren dem Interpreter, an welchen Stellen die entsprechenden Werte oder Ausdrücke eingefügt werden müssen.

## Siehe auch:
- Offizielle Fish Shell Dokumentation zu String-Interpolation: https://fishshell.com/docs/current/index.html#string-interpolation
- Tutorial zur Verwendung von String-Interpolation in Fish Shell: https://dev.to/dwightwatson/advanced-string-interpolation-in-fish-shell-5a78
- Vergleich von String-Interpolation in verschiedenen Programmiersprachen: https://en.wikipedia.org/wiki/String_interpolation#Comparison_table