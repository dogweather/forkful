---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Zusammenfügen von Zeichenketten oder "String-Konkatenation" erlaubt es uns, zwei oder mehr Strings zu einem einzigen String zusammenzuführen. Programmierer nutzen dies, um dynamische Inhalte zu erzeugen oder Informationen aus verschiedenen Quellen zu kombinieren.

## So geht's:

Betrachten wir einige Beispiele mit Gleam:

```Gleam
let str1 = "Hallo, "
let str2 = "Welt!"
io.println(str1 ++ str2)
// Ausgabe: '"Hallo, Welt!"'
```

In Gleam fügen wir Zeichenketten mit `++` zusammen. Die obige Funktion wird `"Hallo, Welt!"` zur Konsole ausgeben.

## Genauer betrachtet

Historisch gesehen ist das Verknüpfen von Zeichenketten in fast allen Sprachen weit verbreitet, obwohl der genaue Mechanismus variiert. Alternativ könnten Sie auch Funktionen wie `String.join` verwenden, wenn Sie mehrere Strings zusammenfügen möchten. Allerdings ist `++` in Gleam die gängige Methode.

Unter der Haube erzeugt Gleam bei der Verwendung von `++` eine neue Zeichenkette, anstatt die ursprünglichen Zeichenketten zu ändern. Dies sichert die Unveränderlichkeit von Daten, ein Grundpfeiler von funktionaler Programmiersprachen wie Gleam.

## Weiterführende Literatur

Weitere Informationen und Beispiele zum Arbeiten mit Zeichenketten in Gleam finden Sie in der offiziellen Gleam-Dokumentation: