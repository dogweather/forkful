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

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann nützlich sein, um unerwünschte Daten zu entfernen oder einen String in einem bestimmten Format zu formatieren. Gleam bietet eine effiziente und einfache Möglichkeit, diese Art von Aufgaben durchzuführen.

## Wie geht das?

```Gleam
String.delete_match("HelloAwesome", "weso") // gibt "HellAwe" zurück
```

Das obige Beispiel zeigt, wie wir mit Gleam einen String verarbeiten und bestimmte Zeichen basierend auf einem Muster löschen können. Der erste Parameter ist der zu verarbeitende String und der zweite Parameter ist das Muster, nach dem gesucht werden soll. Alle Zeichen, die diesem Muster entsprechen, werden aus dem String entfernt.

## Tieferes Eintauchen

Das Löschen von Zeichen basierend auf einem Muster ist dehnbarer als es auf den ersten Blick scheint. Gleam bietet die Möglichkeit, reguläre Ausdrücke zu verwenden, um noch genauere und komplexere Muster zu erstellen. Hier ist ein Beispiel, wie wir mit regulären Ausdrücken arbeiten können, um nur Ziffern aus einem String zu entfernen:

```Gleam
String.delete_match_regex("123jsk45ndoz", "[0-9]") // gibt "jskndoz" zurück
```

## Siehe auch

- Offizielle Gleam Dokumentation: [String Modul](https://gleam.run/documentation/standard-library/string/)
- Reguläre Ausdrücke in Gleam: [Regex Modul](https://gleam.run/documentation/standard-library/regex/)
- Weitere Beispiele und Tutorials finden Sie auf [GitHub](https://github.com/gleam-lang/gleam/tree/master/examples)