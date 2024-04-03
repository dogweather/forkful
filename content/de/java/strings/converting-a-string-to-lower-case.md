---
date: 2024-01-20 17:38:42.079104-07:00
description: "Ein String in Kleinbuchstaben umzuwandeln bedeutet, alle Gro\xDFbuchstaben\
  \ in einem Text durch ihre kleingeschriebenen Gegenst\xFCcke zu ersetzen. Wir tun\
  \ dies,\u2026"
lastmod: '2024-03-13T22:44:53.750525-06:00'
model: gpt-4-1106-preview
summary: "Ein String in Kleinbuchstaben umzuwandeln bedeutet, alle Gro\xDFbuchstaben\
  \ in einem Text durch ihre kleingeschriebenen Gegenst\xFCcke zu ersetzen."
title: Umformung eines Strings in Kleinbuchstaben
weight: 4
---

## So geht’s:
```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String originalString = "Das ist EIN Test!";
        String lowerCaseString = originalString.toLowerCase();

        System.out.println("Original: " + originalString);
        System.out.println("In Kleinbuchstaben: " + lowerCaseString);
    }
}
```

Ausgabe:

```
Original: Das ist EIN Test!
In Kleinbuchstaben: das ist ein test!
```

## Hintergrund:
Die Methode `toLowerCase()` in Java stammt aus den Anfangszeiten von Java. Sie ist Teil der `String`-Klasse und verwendet lokalisierte Regeln, um die Konvertierung auf eine Weise durchzuführen, die für die jeweilige Sprache geeignet ist. Es gibt auch eine Überladung `toLowerCase(Locale locale)`, die es ermöglicht, die Kleinbuchstabenumwandlung basierend auf einem bestimmten Locale-Objekt durchzuführen. Diese Unterscheidung ist wichtig, da bestimmte Sprachen spezielle Regeln für Kleinbuchstaben haben.

Alternativen zur Standardmethodik sind Bibliotheken von Drittanbietern oder das manuelle Durchlaufen der Zeichen in einem String und Anwendung der Kleinbuchstabenregeln. Bei der Implementierung ist zu beachten, dass die `toLowerCase()`-Methode nicht immer "Locale"-neutral ist. Ohne einen `Locale`-Parameter nutzt sie die Standardlokale der Java Virtual Machine, was zu unterschiedlichen Ergebnissen auf unterschiedlichen Systemen führen kann.

## Siehe auch:
- [Java String toLowerCase() Method](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#toLowerCase())
- [Locale-Klassenreferenz in Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Locale.html)
- [Unicode und Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Character.html)
