---
title:                "Eine Zeichenfolge großschreiben"
html_title:           "Java: Eine Zeichenfolge großschreiben"
simple_title:         "Eine Zeichenfolge großschreiben"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?
Capitalizing strings bedeutet, dass alle Anfangsbuchstaben eines Strings in Großbuchstaben umgewandelt werden. Programmierer nutzen dies, um die Lesbarkeit von Texten zu verbessern, insbesondere bei Benutzereingaben oder aus Textdateien.

## So geht's:
```
public class Capitals {
    public static void main(String[] args) {
        String text = "hallo, welt!";
        System.out.println(text.toUpperCase());
    }
}
```
Output:
`HALLO, WELT!`

## Tiefere Einblicke:
Das Umwandeln von Strings in Großbuchstaben hat seinen Ursprung im Druckwesen, wo manuelle Buchstaben in Druckschriften in Großbuchstaben geschrieben wurden. Alternativ können Programmierer auch die Methode `String#capitalize()` verwenden, um nur den ersten Buchstaben eines Strings in einen Großbuchstaben zu ändern. Die Implementierung des Capitalizings in Java erfolgt durch die Verwendung von `Locale`-Einstellungen, um sprachliche Unterschiede zu berücksichtigen.

## Weitere Infos:
[A stackoverflow thread about capitalizing a string](https://stackoverflow.com/questions/225337/how-do-i-capitalize-the-first-letter-of-a-string-in-java)
[A tutorial on the usage of `Locale` in Java](https://www.baeldung.com/java-locale)