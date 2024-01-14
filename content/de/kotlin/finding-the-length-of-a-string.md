---
title:                "Kotlin: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum

Das Finden der Länge eines Strings ist ein häufig verwendeter Prozess bei der Entwicklung von Software. Es ermöglicht Programmierern, die Länge eines Textes oder einer Kombination von Zeichen zu bestimmen, um sie dann für verschiedene Zwecke zu nutzen.

# Wie

In Kotlin ist das Finden der Länge eines Strings sehr einfach. Hier ist ein Beispiel, wie man dies in einer Funktion nutzen kann:

```Kotlin
fun main() {
    val text = "Guten Tag"
    val length = text.length
    
    println("Die Länge des Strings ist: $length")
}

// Output: Die Länge des Strings ist: 9
```

In diesem Beispiel haben wir ein String-Objekt mit dem Wert "Guten Tag" erstellt. Dann haben wir die Länge dieses Strings mithilfe der `length`-Funktion ermittelt und in einer Variable gespeichert. Schließlich geben wir die Länge über die `println`-Funktion aus.

# Deep Dive

Um das Konzept des String-Längen-Findens besser zu verstehen, ist es hilfreich zu wissen, dass ein String in Kotlin im Wesentlichen eine Kette von einzelnen Zeichen darstellt. Die Funktion `length` gibt einfach die Anzahl der Zeichen in diesem String zurück.

Eine wichtige Sache zu beachten ist, dass die `length`-Funktion nicht nur bei normalen Strings, sondern auch bei anderen Kotlin-Datentypen wie beispielsweise Arrays verwendet werden kann.

# Siehe auch

- Offizielle Dokumentation zu Strings und deren Größe in Kotlin: https://kotlinlang.org/docs/reference/basic-types.html#strings
- Einführung in Kotlin: https://developer.android.com/kotlin/learn
- Weitere nützliche Funktionen bei der Arbeit mit Strings in Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/