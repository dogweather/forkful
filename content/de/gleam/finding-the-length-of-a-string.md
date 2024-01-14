---
title:                "Gleam: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings kann eine wertvolle Fähigkeit sein, wenn man mit Texten in der Programmierung arbeitet. Es kann Ihnen helfen, effizienter zu arbeiten und Fehler in Ihrem Code zu vermeiden. Lassen Sie uns sehen, wie es mit Gleam gemacht werden kann.

## Wie man die Länge eines Strings in Gleam findet

```Gleam
let string = "Dies ist ein Beispieltext"
let length = String.length(string)
``` 

In diesem Codeblock definieren wir einen String mit dem Wert "Dies ist ein Beispieltext" und verwenden dann die String.length-Funktion, um die Länge des Strings zu bestimmen. Das Ergebnis wird der Variablen "length" zugewiesen, die wir dann ausgeben können. 

Die Ausgabe dieses Codes wird "23" sein, da der String insgesamt 23 Zeichen lang ist (einschließlich Leerzeichen). Sie können dies auch mit anderen Strings ausprobieren, um das Ergebnis zu überprüfen.

## Tiefer Einblick

Das Finden der Länge eines Strings mag einfach erscheinen, aber es gibt einige wichtige Dinge zu beachten. Zum Beispiel kann die Länge eines Strings in verschiedenen Programmiersprachen unterschiedlich berechnet werden, je nachdem, ob Leerzeichen mitgezählt werden oder nicht.

In Gleam zählen Leerzeichen standardmäßig mit, was für die meisten Anwendungen ausreichend ist. Wenn Sie jedoch spezielle Anforderungen haben, können Sie die Standardfunktion überschreiben und Ihre eigene Implementierung schreiben.

Außerdem ist es wichtig, zu verstehen, dass der Begriff "Länge" auf die Anzahl der Zeichen im String, nicht auf die Anzahl der Wörter, bezogen ist. Dies kann verwirrend sein, wenn der String aus zusammengesetzten Wörtern besteht, aber die Funktion berechnet immer noch die Gesamtzahl der Zeichen.

## Siehe auch

- [Gleam Dokumentation zu String.length](https://gleam.run/documentation/stdlib/string.html#length)
- [GeeksForGeeks-Artikel über die Länge eines Strings](https://www.geeksforgeeks.org/length-string-in-rust/)
- [Freecodecamp-Tutorial zu Strings in Gleam](https://www.freecodecamp.org/news/writing-real-gleam-code-strings-and-formatting/)