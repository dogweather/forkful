---
title:                "Zeichenketten verknüpfen"
aliases: - /de/kotlin/concatenating-strings.md
date:                  2024-01-20T17:34:58.019054-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten verknüpfen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
String-Konkatenation bedeutet, zwei oder mehr Strings zu einem zusammenzufügen. Programmierer machen das, um dynamische Textausgaben zu erzeugen oder Daten zu formatieren.

## How to: (Wie macht man das:)
```kotlin
fun main() {
    val hello = "Hallo"
    val world = "Welt"
    val exclamation = "!"

    // Verwendung des Plus-Operators
    val greeting1 = hello + " " + world + exclamation
    println(greeting1) // Output: Hallo Welt!

    // String Templates nutzen
    val greeting2 = "$hello $world$exclamation"
    println(greeting2) // Output: Hallo Welt!

    // StringBuilder für größere Operationen
    val stringBuilder = StringBuilder(hello).append(" ").append(world).append(exclamation)
    println(stringBuilder) // Output: Hallo Welt!
}
```

## Deep Dive (Tiefer eintauchen)
Historisch gesehen, war String-Konkatenation immer ein Teil der Programmierung, da der Umgang mit Text zentral ist. Früher, besonders in Sprachen wie Java, konnte Konkatenation teuer bezüglich der Performance sein, da Strings unveränderlich waren. Jede Konkatenation erzeugte ein neues String-Objekt. Kotlin verbessert dies durch Verwendung von `StringBuilder` im Hintergrund beim Verwenden des `+` Operators.

Alternativen zur Verkettung von Strings umfassen das Joinen von Kollektionen (`joinToString`) oder Streams in modernen Java-Versionen und Kotlin. Im Performance-kritischen Kontext empfiehlt sich `StringBuilder`.

In Bezug auf Implementierungsdetails ist es hilfreich zu wissen, dass der Kotlin-Compiler String-Konkatenationen erkennt und optimiert. Bei einfachen Fällen benutzt er den `+` Operator und bei komplexeren inneren Schleifen oder großen String-Operationen setzt er den `StringBuilder` ein.

## See Also (Siehe auch)
- Kotlin Dokumentation zu Strings: [Strings | Kotlin](https://kotlinlang.org/docs/strings.html)
- Oracle Java Tutorials zu StringBuilder: [StringBuilder (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html) 
- Kotlin API Referenz zu StringBuilder: [StringBuilder | Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)
