---
title:                "Java: Umwandlung eines Variablenwerts in Kleinbuchstaben"
simple_title:         "Umwandlung eines Variablenwerts in Kleinbuchstaben"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Zeichenketten in Kleinbuchstaben ist ein grundlegendes Konzept in der Programmierung und wird in vielen Fällen benötigt. Es kann beispielsweise helfen, die Vergleichbarkeit von Texten zu verbessern oder die Eingabe von Benutzern zu vereinfachen.

## Wie geht das?

```Java
// Initialisiere String mit Großbuchstaben
String text = "DIE SONNE SCHEINT";
System.out.println(text); // Ausgabe: DIE SONNE SCHEINT

// Konvertiere String in Kleinbuchstaben
String converted = text.toLowerCase();
System.out.println(converted); // Ausgabe: die sonne scheint
```

Wir verwenden die Methode `toLowerCase()` auf einem String-Objekt, um alle Großbuchstaben in Kleinbuchstaben zu konvertieren. Dies betrifft auch Umlaute und Sonderzeichen.

## Tiefenschärfe

Hinter den Kulissen wird jedes Zeichen durch einen numerischen Wert dargestellt, der als Unicode bezeichnet wird. Jeder Buchstabe, jedes Symbol und jede Ziffer hat einen eindeutigen Code, der verwendet wird, um es darzustellen. Bei Groß- und Kleinbuchstaben gibt es einen numerischen Unterschied von 32, was es uns ermöglicht, durch einfaches Hinzufügen von 32 in den ASCII-Wert jedes Großbuchstabens den entsprechenden Kleinbuchstaben zu erhalten.

Man könnte auch erwarten, dass es eine Methode `toUpperCase()` gibt, die die Zeichen in Großbuchstaben konvertiert. Tatsächlich gibt es diese Methode auch in Java, aber sie hat ihre eigenen Besonderheiten, die es zu beachten gilt.

## Siehe auch

- [Java String-Methoden](https://www.geeksforgeeks.org/string-class-in-java/)
- [ASCII und Unicode](https://www.w3schools.com/charsets/ref_html_ascii.asp)