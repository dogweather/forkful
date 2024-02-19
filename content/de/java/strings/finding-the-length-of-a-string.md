---
aliases:
- /de/java/finding-the-length-of-a-string/
date: 2024-01-20 17:47:32.852713-07:00
description: "Die L\xE4nge eines Strings zu ermitteln bedeutet, zu z\xE4hlen, wie\
  \ viele Zeichen er enth\xE4lt. Entwickler brauchen diese Information, um Eingaben\
  \ zu validieren,\u2026"
lastmod: 2024-02-18 23:09:04.727511
model: gpt-4-1106-preview
summary: "Die L\xE4nge eines Strings zu ermitteln bedeutet, zu z\xE4hlen, wie viele\
  \ Zeichen er enth\xE4lt. Entwickler brauchen diese Information, um Eingaben zu validieren,\u2026"
title: "Ermittlung der Zeichenkettenl\xE4nge"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge eines Strings zu ermitteln bedeutet, zu zählen, wie viele Zeichen er enthält. Entwickler brauchen diese Information, um Eingaben zu validieren, Daten zu bearbeiten oder einfach die Größe eines Textes zu bestimmen.

## How to:
Java macht es leicht, die Länge eines Strings zu erlangen – benutze einfach die `length()` Methode.

```java
public class StringLengthExample {
    public static void main(String[] args) {
        String myString = "Hallo Welt!";
        int length = myString.length();
        System.out.println("Die Länge des Strings ist: " + length);
    }
}
```

Ausgabe:

```
Die Länge des Strings ist: 11
```

## Deep Dive:
Früher in Sprachen wie C, musste man selbst durch einen String iterieren, um dessen Länge zu ermitteln. Java's `length()` Methode verdeckt diese Komplexität. Alternativ könnten Entwickler die `chars()` Stream-API nutzen, die mit Java 8 eingeführt wurde, um Zeichen oder Zeichenkombinationen zu zählen, besonders bei Unicode-Texten, wo `.length()` trügerisch sein kann.

Ein Beispiel mit `chars()` um die Anzahl der Buchstaben 'l' in einem String zu zählen:

```java
public class CharacterCountExample {
    public static void main(String[] args) {
        String myString = "Hallo Welt!";
        long count = myString.chars().filter(ch -> ch == 'l').count();
        System.out.println("Anzahl der 'l': " + count);
    }
}
```

Ausgabe:

```
Anzahl der 'l': 3
```

`length()` funktioniert, weil ein `String` in Java intern als Array von `char` Werten implementiert ist, und `length()` einfach die Array-Länge zurückgibt. Achtung: Bei Unicode-Zeichen, die durch mehr als ein `char` repräsentiert sind (also Surrogat-Paare), gibt `length()` nicht die tatsächliche Anzahl der Zeichen zurück. `codePointCount()` kann in solchen Fällen genutzt werden.

## See Also:
- [Java String documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Oracle's Java tutorials for Strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Unicode und Java](https://www.oracle.com/technical-resources/articles/javase/supplementary.html)
