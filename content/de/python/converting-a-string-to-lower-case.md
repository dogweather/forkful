---
title:                "Python: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man Strings in Python in Kleinbuchstaben konvertieren möchte. Einer der wichtigsten Gründe ist die Verarbeitung und Vergleichung von Daten, um sicherzustellen, dass alle Buchstaben auf die gleiche Weise formatiert sind und somit Vergleiche korrekt durchgeführt werden können.

## Wie

Die Konvertierung eines Strings in Kleinbuchstaben ist in Python sehr einfach. Hier ist ein Beispielcode, der dies demonstriert:

```Python
# Erstelle einen String
string = "HALLO, ICH BIN EINE STRING"

# Konvertiere in Kleinbuchstaben und speichere es in einer neuen Variablen
neuer_string = string.lower()

# Gib den neuen String aus
print(neuer_string)
```

Die Ausgabe dieses Codes wäre: "hallo, ich bin eine string". Wie Sie sehen können, wurden alle Buchstaben in Kleinbuchstaben umgewandelt.

## Tiefere Einblicke

Die `lower()` Methode, die auf Strings angewendet wird, verwendet die Standard Unicode Datenbank des Computers, um jeden Buchstaben in Kleinbuchstaben zu konvertieren. Dies bedeutet, dass auch Zeichen aus anderen Sprachen, die in der Unicode Datenbank enthalten sind, richtig umgewandelt werden.

Es ist auch wichtig zu wissen, dass die `lower()` Methode den ursprünglichen String nicht ändert, sondern stattdessen einen neuen String mit den konvertierten Buchstaben zurückgibt.

## Siehe auch

- [Python String Methoden](https://www.python-kurs.eu/python3_string_methods.php)
- [Unicode Datenbank](https://unicode.org/faq/utf_bom.html)