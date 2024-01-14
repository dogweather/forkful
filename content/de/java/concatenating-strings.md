---
title:                "Java: Verbinden von Zeichenketten"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum
Das Zusammenfügen von Zeichenketten (auch bekannt als String-Konkatenation) ist eine wichtige Technik in der Java-Programmierung. Durch das Verbinden von einzelnen Zeichenketten können wir längere Sätze oder komplexere Ausgaben erzeugen, die in unseren Programmen verwendet werden können.

## How To
In Java gibt es mehrere Möglichkeiten, Zeichenketten zu kombinieren. Eine häufige Methode ist die Verwendung des '+' Operators, um zwei oder mehr Strings zusammenzufügen. Zum Beispiel:

```Java
String name = "Lisa";
String greeting = "Hallo ";

System.out.println(greeting + name);
```

Die Ausgabe dieses Codes wäre "Hallo Lisa". Wir können auch den `+=` Operator verwenden, um auf einfache Weise einen String an einen vorhandenen String anzuhängen, wie im folgenden Beispiel:

```Java
String sentence = "Ich bin ";
sentence += "ein Java-Entwickler";
```

Die Ausgabe dieses Codes wäre "Ich bin ein Java-Entwickler". Wir können sogar mehrere Variablen miteinander verbinden, indem wir sie zwischen den '+' Operatoren einfügen:

```Java
String adjective = "großartiger";
String noun = "Programmierer";
String intro = "Ich bin ein ";

System.out.println(intro + adjective + " " + noun);
```

Dieser Code würde "Ich bin ein großartiger Programmierer" ausgeben.

## Deep Dive
Bei der Verwendung von Zeichenkettenverkettung ist zu beachten, dass Java eine spezielle Methode namens `concat()` hat, die speziell für die Kombination von Zeichenketten entwickelt wurde. Diese Methode akzeptiert eine Zeichenkette als Parameter und verbindet sie mit der aktuellen Zeichenfolge. Es ist jedoch wichtig zu beachten, dass das Verwenden von `concat()` im Gegensatz zu '+' keine automatische Typumwandlung zwischen Zeichenketten und anderen Datentypen durchführt.

Außerdem ist beim Zusammenfügen von Zeichenketten Vorsicht geboten, da dies bei größeren Mengen von Strings zu einer Verschwendung von Speicherplatz führen kann. In solchen Fällen ist es empfehlenswert, die `StringBuilder` Klasse zu verwenden, die effizienter für das Zusammenfügen großer Mengen von Zeichenketten ist.

## Siehe auch
- [Java Strings](https://www.w3schools.com/java/java_strings.asp)
- [Java StringBuilder](https://www.w3schools.com/java/java_stringbuilder.asp)
- [Java String concat()](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#concat(java.lang.String))