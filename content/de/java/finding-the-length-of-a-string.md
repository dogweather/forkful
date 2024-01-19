---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Länge eines Strings zu ermitteln ist die Aktion, die Anzahl der Zeichen in einem bestimmten String zu bestimmen. Programmierer machen dies oft, um beim Durchsuchen oder Verändern von Strings genaue Berechnungen durchzuführen.

## So geht's:

Hier ist ein einfaches Beispiel, wie man die Länge eines Strings in Java findet. 

```Java
public class Main{
    public static void main(String[] args) {
        String str = "Hallo Welt";
        int length = str.length();
        System.out.println("Die Länge des Strings ist: " + length);
    }
}
```

Die Ausgabe dieses Codes wird sein:

`Die Länge des Strings ist: 10`

## Tiefgehende Details:

1. Historischer Kontext: Die Methode .length() ist seit der ersten Version von Java vorhanden und ermöglicht es Entwicklern, die Anzahl der Zeichen in einem String effizient zu zählen.
2. Alternativen: In manchen Fällen kann man die Methode .isEmpty() verwenden, um zu überprüfen, ob ein String leer ist (d.h., seine Länge ist 0).
3. Implementierungsdetails: Die Methode .length() gibt die Anzahl der 16-Bit-Unicode-Zeichen im String zurück.

## Siehe auch:

1. Java String Documentation: https://docs.oracle.com/javase/10/docs/api/java/lang/String.html
2. Oracle Java Tutorial, Strings: https://docs.oracle.com/javase/tutorial/java/data/strings.html
3. Java String length() Method with Examples: https://www.javatpoint.com/java-string-length