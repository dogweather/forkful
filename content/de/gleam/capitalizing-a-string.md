---
title:    "Gleam: Ein String großschreiben"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Warum

Es gibt viele Situationen, in denen man einen Text in Großbuchstaben schreiben möchte. Zum Beispiel bei der Eingabe von Passwörtern, in Überschriften oder zur Betonung bestimmter Wörter. Mit Gleam ist es ganz einfach, einen String in Großbuchstaben zu konvertieren. In diesem Blogbeitrag zeige ich dir, wie es geht!

## Wie man einen String in Gleam in Großbuchstaben umwandelt

Um einen String in Gleam in Großbuchstaben zu schreiben, musst du die Funktion `String.to_upper` verwenden. Diese Funktion akzeptiert einen beliebigen String und gibt den entsprechenden String in Großbuchstaben zurück.

```Gleam
String.to_upper("Hallo, Berlin!")
```

Das Ergebnis dieser Funktion wäre `"HALLO, BERLIN!"`. Wie du sehen kannst, werden alle Buchstaben im String in Großbuchstaben umgewandelt.

## Tiefergehende Erklärung

Die `String.to_upper` Funktion verwendet das UTF-8-Zeichensatzformat, um sicherzustellen, dass auch Sonderzeichen und Umlaute korrekt in Großbuchstaben umgewandelt werden. Außerdem arbeitet die Funktion mit einfachen Unicode-Zeichencodes anstelle von Zeichen als solchen, was bedeutet, dass sie in der Lage ist, mit komplexen Texten umzugehen, die mehr als eine Byte-Gruppe pro Zeichen enthalten.

## Siehe auch

- Dokumentation zu Gleam Strings: https://gleam.run/documentation/stdlib/string/
- Offizielle Gleam Website: https://gleam.run/
- Beispielcode für Gleam: https://github.com/gleam-lang/gleam/tree/master/examples