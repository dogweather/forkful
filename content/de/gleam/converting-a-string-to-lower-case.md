---
title:    "Gleam: Eine Zeichenfolge in Kleinbuchstaben umwandeln"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Warum

Die Umwandlung von Strings in Kleinbuchstaben ist ein grundlegendes Konzept in der Programmierung und kann in verschiedenen Szenarien nützlich sein. Zum Beispiel, wenn wir Benutzereingaben validieren möchten, ist es wichtig, dass wir sicherstellen, dass die Groß- und Kleinschreibung keine Rolle spielt. Deshalb ist es wichtig, zu wissen, wie man in Gleam Strings in Kleinbuchstaben umwandelt.

## So geht's

Die Umwandlung von String in Kleinbuchstaben in Gleam ist ganz einfach! Im Folgenden zeigen wir dir, wie es geht:

```Gleam
input = "Hallo Welt"
output = String.to_lower(input)
```

Der obige Code verwendet die eingebaute Funktion `String.to_lower`, um den String `input` in Kleinbuchstaben zu konvertieren. Das Ergebnis wird in der Variable `output` gespeichert.

**Output:**

```Gleam
"hallo welt"
```

## Tiefer Tauchgang

Es ist wichtig zu verstehen, dass die Umwandlung von Strings in Kleinbuchstaben von der verwendeten Codierung abhängt. In den meisten Fällen sollte Gleam jedoch automatisch die richtige Methode wählen, um Strings in Kleinbuchstaben umzuwandeln, unabhängig von der verwendeten Codierung. Dies macht es einfach und effizient, Strings in Gleam zu manipulieren.

## Siehe auch

- [Gleam-Dokumentation zu Strings](https://gleam.run/book/core/strings.html)
- [Gleam-Tutorial zu Strings](https://gleam.run/book/tutorials/strings.html)