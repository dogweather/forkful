---
date: 2024-01-20 17:35:21.399140-07:00
description: "String-Konkatenation ist das Zusammenf\xFCgen von zwei oder mehreren\
  \ Zeichenketten (Strings) zu einer neuen Zeichenkette. Wir brauchen das, um dynamische\u2026"
lastmod: '2024-03-13T22:44:53.755273-06:00'
model: gpt-4-1106-preview
summary: "String-Konkatenation ist das Zusammenf\xFCgen von zwei oder mehreren Zeichenketten\
  \ (Strings) zu einer neuen Zeichenkette. Wir brauchen das, um dynamische\u2026"
title: "Zeichenketten verkn\xFCpfen"
weight: 3
---

## Was & Warum?

String-Konkatenation ist das Zusammenfügen von zwei oder mehreren Zeichenketten (Strings) zu einer neuen Zeichenkette. Wir brauchen das, um dynamische Texte zu erzeugen, Daten zu formatieren oder einfach Nachrichten zusammenzubauen.

## So geht's:

Hier sind einige schnelle Beispiele, wie man Strings in Java konkatenieren kann:

```java
public class StringConcatenationDemo {
    public static void main(String[] args) {
        String hello = "Hallo";
        String world = "Welt";
        String exclamation = "!";
        
        // Verwendung des + Operators
        String message = hello + ", " + world + exclamation;
        System.out.println(message);  // Ausgabe: Hallo, Welt!
        
        // Verwendung der concat() Methode
        String anotherMessage = hello.concat(", ").concat(world).concat(exclamation);
        System.out.println(anotherMessage);  // Ausgabe: Hallo, Welt!
        
        // Verwendung von StringBuilder
        StringBuilder sb = new StringBuilder();
        sb.append(hello).append(", ").append(world).append(exclamation);
        System.out.println(sb.toString());  // Ausgabe: Hallo, Welt!
    }
}
```

## Tiefere Einblicke:

Historisch gesehen wurde die String-Konkatenation häufig mit dem `+` Operator durchgeführt. Doch bei einer großen Anzahl von String-Operationen kann das ineffizient sein, da Strings in Java unveränderlich (immutable) sind und so bei jeder Konkatenation neue Objekte erzeugt werden.

Als Alternative und für leistungsstärkere Konkatenationen bieten sich Klassen wie `StringBuilder` oder `StringBuffer` an. Diese ermöglichen es, größere Mengen von Strings ohne ständige Neuerstellung von String-Objekten zusammenzusetzen. `StringBuilder` ist dabei die schnellere, jedoch nicht threadsichere Variante.

Eine andere Option wäre die Nutzung der `concat()` Methode von String-Objekten, deren Verbreitung durch den einfacher schreibbaren `+` Operator aber begrenzt ist.

Java 8 hat den `StringJoiner` und in Java 12 die `indent` und `transform` Methoden eingeführt, die weitere Möglichkeiten der Textmanipulation bieten. Die Wahl der Methode hängt von der spezifischen Anforderung und der beabsichtigten Lesbarkeit bzw. Leistung ab.

## Siehe auch:

- Die offizielle Java-Dokumentation zu `StringBuilder`: https://docs.oracle.com/javase/10/docs/api/java/lang/StringBuilder.html
- Eine Diskussion zu String-Konkatenation in Stack Overflow: https://stackoverflow.com/questions/1532461/stringbuilder-vs-string-concatenation-in-tostring-in-java
- Ein Guide zu Strings in Java: https://www.baeldung.com/java-strings
