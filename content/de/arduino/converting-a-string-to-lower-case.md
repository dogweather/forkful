---
title:                "Umwandeln einer Zeichenkette in Kleinbuchstaben"
html_title:           "Arduino: Umwandeln einer Zeichenkette in Kleinbuchstaben"
simple_title:         "Umwandeln einer Zeichenkette in Kleinbuchstaben"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Was & Warum?
"Converting a string to lower case" bedeutet, dass man alle Buchstaben einer Zeichenkette (String) in Kleinbuchstaben umwandelt. Das wird von Programmierern oft gemacht, um Vergleichsoperationen einfacher zu gestalten oder um eine einheitliche Formatierung zu erreichen.

# Wie geht das?
Um eine Zeichenkette in Kleinbuchstaben umzuwandeln, gibt es verschiedene Ansätze. Hier ist ein Beispiel in Arduino Code:

```
// Beispiel String
String text = "Die Sonne scheint!";
// Umwandlung in Kleinbuchstaben
text.toLowerCase();
// Ausgabe
Serial.println(text);
// Ergebnis: "die sonne scheint!"
```

# Tiefeneintauchen
Die Umwandlung von Strings in Kleinbuchstaben ist ein häufig verwendetes Konzept in der Programmierung und wird in vielen Programmiersprachen unterstützt. Alternativ kann man auch jeden einzelnen Buchstaben manuell umwandeln, was jedoch zeitaufwändiger ist. Bei der Implementierung ist zu beachten, dass je nach Programmiersprache und Zeichensatz auch Sonderzeichen unterschiedlich behandelt werden können.

# Siehe auch
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Wikipedia: Zeichenkonvertierung](https://de.wikipedia.org/wiki/Zeichenkonvertierung)