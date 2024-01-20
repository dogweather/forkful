---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von Zeichen, die einem Muster entsprechen, ermöglicht es uns, spezifische Daten aus einer größeren Datengruppe zu entfernen. Wir tun dies, um die Datenbereinigung und -manipulation zu erleichtern und zu verbessern, insbesondere wenn wir nur bestimmte Daten aus einer Gruppe benötigen.

## So geht's:
Lasst uns ein Beispiel im Fish Shell Codeblock betrachten:

```Fish Shell
# Angenommen, Sie haben die folgende Zeichenkette:
set string 'Hallo, fish shell!'

# Sie können Zeichen löschen, die einem Muster entsprechen, zum Beispiel:
echo $string | string replace -ra '[,!]' ''

# Ausgabe:
Hallo fish shell
```
In diesem Beispiel haben wir alle Kommata und Ausrufezeichen aus der Zeichenkette entfernt.

## Vertiefung:
Das Löschen von Zeichen, die einem Muster entsprechen, hat seine Wurzeln in den Regular Expressions (RegEx). RegEx unterstützt komplexe Mustererkennungs- und Manipulationsfähigkeiten. Andere Shell-Skriptsprachen wie Bash haben ihre eigene Syntax und Methoden zum Löschen von Zeichen.

Es gibt auch andere Methoden im Fish Shell zum Löschen von Zeichen. Eine davon ist die `string match` Funktion, die wir mit der `-v` Option (invert) verwenden können, um die nicht übereinstimmenden Zeichen beizubehalten.

Implementation Details: Fish Shell verwendet Internally Regular Expressions, um Muster zu erkennen und dann die `string replace` Funktion, um die entsprechenden Zeichen zu entfernen.

## Siehe auch:
Für zusätzliche Informationen, schaut euch die Dokumentation und Wikis an:

- Fish Shell Dokumentation zur `string replace` Funktion: https://fishshell.com/docs/3.1/cmds/string-replace.html
- Ein tieferer Einblick in RegEx: https://www.regular-expressions.info/
- Fish Shell Wiki und Gemeinschaft: https://github.com/fish-shell/fish-shell/wiki