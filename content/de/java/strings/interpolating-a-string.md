---
date: 2024-01-20 17:51:01.158472-07:00
description: "How to: Java bietet keine eingebaute String-Interpolation wie einige\
  \ andere Sprachen. Aber seit Java 5 verwenden wir `printf` oder `String.format`.\
  \ Ab\u2026"
lastmod: '2024-03-13T22:44:53.749649-06:00'
model: gpt-4-1106-preview
summary: Java bietet keine eingebaute String-Interpolation wie einige andere Sprachen.
title: Zeichenketten interpolieren
weight: 8
---

## How to:
Java bietet keine eingebaute String-Interpolation wie einige andere Sprachen. Aber seit Java 5 verwenden wir `printf` oder `String.format`. Ab Java 8 können wir auch die `MessageFormat`-Klasse nutzen.

```java
public class StringInterpolationBeispiel {
    public static void main(String[] args) {
        String name = "Welt";
        int anzahl = 3;

        // Mit String.format
        String begruessung = String.format("Hallo %s! Ich sehe dich zum %d. Mal.", name, anzahl);
        System.out.println(begruessung);

        // Mit printf
        System.out.printf("Hallo %s! Ich sehe dich zum %d. Mal.%n", name, anzahl);
    }
}
```

Ausgabe:
```
Hallo Welt! Ich sehe dich zum 3. Mal.
Hallo Welt! Ich sehe dich zum 3. Mal.
```

## Deep Dive:
Historisch gesehen, bevorzugten Java-Entwickler die String-Konkatenation mit dem `+`-Operator. Nach der Einführung von `printf` und `String.format` in Java 5 adaptierte die Gemeinschaft schnell die Formatierungsmöglichkeiten im Stil von `printf` aus C.

Alternativen zu `String.format`:
- `StringBuilder` oder `StringBuffer`: Für komplexe oder performance-kritische Stringoperationen.
- `MessageFormat`: Bietet erweiterte Funktionen wie z.B. wählen von Textformen je nach Zahl (Mehrzahl, Einzahl).

Implementierungsdetails:
- `String.format` und `printf` verwenden intern `Formatter`.
- Die Performance von `String.format` und `printf` kann geringer sein als die von `StringBuilder`-Operationen.

## See Also:
- [Java String.format Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#format-java.lang.String-java.lang.Object...-)
- [MessageFormat Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/text/MessageFormat.html)
- [StringBuilder Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
