---
title:                "Kotlin: Lesen von Befehlszeilenargumenten"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Die Verwendung von Befehlszeilenargumenten in der Programmierung ist eine effektive Möglichkeit, um die Interaktivität und Flexibilität von Programmen zu erhöhen. Durch das Lesen von Befehlszeilenargumenten kann ein Benutzer verschiedene Varianten eines Programms ausführen, ohne den Programmcode zu ändern. Lesen von Befehlszeilenargumenten ist daher eine wichtige Fähigkeit für jeden Programmierer.

## Wie geht's

Das Lesen von Befehlszeilenargumenten in Kotlin ist schnell und einfach. Im Folgenden ist ein Beispielcode, der das Konzept verdeutlicht:

```Kotlin
fun main(args: Array<String>) {
    val name = args[0] // erster Parameter wird in 'name' gespeichert
    val age = args[1].toInt() // zweiter Parameter als Integer gespeichert

    println("Hallo $name, du bist $age Jahre alt.")
}
```
Angenommen, der Benutzer gibt beim Ausführen des Programms folgende Befehlszeilenargumente ein:

```bash
kotlin Meike 24
```

Die Ausgabe wäre:

```
Hallo Meike, du bist 24 Jahre alt.
```

Die Befehlszeilenargumente werden in einem String-Array `args` gespeichert. Wir können dann auf die einzelnen Argumente zugreifen, indem wir den Index des Arrays angeben. Beachten Sie auch, dass wir den String-Parameter in einen Integer umwandeln, indem wir die `toInt()`-Funktion verwenden.

## Tiefes Eintauchen

Es gibt noch viele andere Möglichkeiten, Befehlszeilenargumente in Kotlin zu lesen und zu verarbeiten. Zum Beispiel können wir überprüfen, ob der Benutzer überhaupt Argumente eingegeben hat, oder wir können benannte Argumente verwenden, um die Eingabe noch flexibler zu gestalten. Wenn Sie tiefer eintauchen möchten, empfehle ich die offizielle Dokumentation von Kotlin zu diesem Thema zu lesen.

## Siehe auch

- [Kotlin-Dokumentation: Befehlszeilenargumente](https://kotlinlang.org/docs/reference/command-line.html)
- [Tutorial: Befehlszeilenargumente in Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_command_line_arguments.htm)
- [Lesen von Befehlszeilenargumenten: Ein praktisches Beispiel in Kotlin](https://medium.com/factory-mind/command-line-arguments-in-kotlin-b478abac56d3)