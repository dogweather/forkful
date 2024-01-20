---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Gleam Programmieren: Zeichen löschen, die einem Muster entsprechen

## Was & Warum?
Durch das Löschen von Zeichen, die einem Muster entsprechen, entfernen wir spezifische Zeichen aus einer Zeichenkette. Programmierer nutzen das, um Daten zu bereinigen oder zu formatieren.

## So geht's:
Mit Gleam können wir das mit dem Modul `gleam/string` und der Funktion `contains` erreichen.

```Gleam
import gleam/string
let my_string = "Gleam Programmierung ist super! ##"
let pattern = "#"
let cleared_string = string.replace(my_string, pattern, "")
```

Der ausgegebene Wert von `cleared_string` wäre: "Gleam Programmierung ist super! ".

## Tieferer Einblick
Historisch gesehen haben Programmierer immer wieder Wege gebraucht, um Muster in Zeichenketten zu identifizieren und zu löschen. Gleam hat das einfach gemacht, mit dem `string.replace`-Funktion.

Alternativen dazu in Gleam könnten Reguläre Ausdrücke `regex` oder der `slice`-Befehl sein. Doch sie können komplexer sein und stark von der genauen Anwendung abhängen. 

Bei der Implementierung von `string.replace` wird durch die Zeichenkette iteriert und jede Instanz des Musters durch einen alternativen String (in diesem Fall ein leerer String) ersetzt. 

## Siehe auch
Für weitere Informationen, besuchen Sie die offizielle Gleam-Dokumentation [hier](https://gleam.run/docs/). Besonders das Modul `gleam/string` kann dabei helfen, mehr über die Möglichkeiten in Gleam zur Manipulation von Zeichenketten zu erfahren. Ein weiterer hilfreicher Link ist [Regular Expressions in Gleam](https://gleam.run/tour/pattern-matching), der weitere Techniken zur Zeichenkettenmanipulation erklärt.