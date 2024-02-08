---
title:                "Teilstrings extrahieren"
date:                  2024-01-20T17:45:56.160571-07:00
model:                 gpt-4-1106-preview
simple_title:         "Teilstrings extrahieren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Extrahieren von Teilstrings bedeutet, spezifische Abschnitte aus einem längeren String herauszuziehen. Programmierer nutzen dies, um Daten zu manipulieren, zu validieren oder um spezifische Informationen zu verarbeiten.

## Anleitung:
Java macht es einfach, Teilstrings zu gewinnen. Hier ein paar Beispiele, wie man das bewerkstelligt:

```java
public class SubstringExample {
    public static void main(String[] args) {
        String text = "Hallo, Welt!";
        
        String teilString1 = text.substring(7); // Ab dem 8. Zeichen (Index 7)
        System.out.println(teilString1); // Ausgabe: "Welt!"
        
        String teilString2 = text.substring(0, 5); // Vom Start bis zum 5. Zeichen (Index 5, exklusiv)
        System.out.println(teilString2); // Ausgabe: "Hallo"
    }
}
```

## Deep Dive:
Das Extrahieren von Teilstrings gibt's in Java schon ewig – seit den Anfängen. Historisch hatten wir die Methoden `substring(int beginIndex)` und `substring(int beginIndex, int endIndex)`, die teilweise unterschiedlich implementiert waren. Vor Java 7, Update 6, waren substrings intern Referenzen auf denselben `char`-Array des Originale Strings mit unterschiedlichen Start- und Endpunkten, was Speichernutzung effizient machte, aber zu Speicherlecks führen konnte, wenn große Strings im Speicher gehalten wurden, auch wenn nur ein kleiner Teilstring benötigt wurde.

In neueren Java-Versionen wird mit der `substring`-Methode ein neuer `String` erzeugt, was diese Problematik umgeht. Alternativ zu substring kann man auch mit `split`, `Pattern` und `Matcher` arbeiten, wenn man komplexere Zerteilungen vornehmen möchte.

## Siehe auch:
- [String.substring in der Java-Dokumentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#substring(int,int))
- [Pattern und Matcher in der Java-Dokumentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [String.split in der Java-Dokumentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#split(java.lang.String))
