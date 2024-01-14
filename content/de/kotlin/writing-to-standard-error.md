---
title:    "Kotlin: Schreiben auf den Standardfehler"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum

Ein wichtiger Aspekt der Programmierung ist die Fehlerbehandlung. Es wird oft empfohlen, das Standardfehler-Ausgabegerät (auch bekannt als Standardfehler-Stream) zu nutzen, um Fehler und Ausnahmen effektiv zu verfolgen und zu handhaben.

# Wie es geht

Die Verwendung des Standardfehler-Streams in Kotlin ist sehr einfach. Wir können die `System.err` Variable verwenden, um auf den Standardfehler-Stream zuzugreifen und dann die `println()` Funktion verwenden, um unsere Fehlermeldungen auszugeben.

```Kotlin
fun main() {
    val num = 0
    try {
        val result = 10 / num
        println(result)
    } catch (e: ArithmeticException) {
        System.err.println("Fehler: Division durch 0")
    }
}
```

Der obige Code versucht, die Division von 10 durch die Variable `num` durchzuführen, die den Wert `0` hat. Dies führt zu einer `ArithmeticException`, die wir in der `catch`-Klausel behandeln. Dort geben wir eine Fehlermeldung über den Standardfehler-Stream aus, um den Benutzer auf die Ausnahme aufmerksam zu machen.

Wenn wir dieses Programm ausführen, erhalten wir die folgende Ausgabe auf dem Standardfehler-Stream:

```
Fehler: Division durch 0
```

Auf diese Weise können wir effektiv Fehler und Ausnahmen verfolgen und behandeln, um unsere Anwendungen robuster zu machen.

# Tiefere Einblicke

Eine Sache, die Sie beachten sollten, ist, dass der Standardfehler-Stream ein eigenständiger Stream ist, der unabhängig vom Standardausgabestream arbeitet. Dies bedeutet, dass Ausgaben auf dem Standardfehler-Stream und dem Standardausgabestream getrennt verarbeitet und angezeigt werden.

Eine weitere wichtige Sache ist, dass der Standardfehler-Stream normalerweise rot dargestellt wird, um auf kritische Fehler hinzuweisen, während der Standardausgabestream normalerweise schwarz dargestellt wird. Dies macht es für den Benutzer einfacher, zwischen normalen Programmausgaben und Fehlermeldungen zu unterscheiden.

# Siehe auch

- [Offizielle Kotlin Dokumentation zu System.err](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-output-stream-writer/system.-err.html)
- [Medium-Artikel zum Umgang mit Fehlern in Kotlin](https://medium.com/swlh/handling-errors-in-kotlin-131b1b6ed075)
- [Stack Overflow Thread zum Thema Fehlerbehandlung in Kotlin](https://stackoverflow.com/questions/51871622/kotlin-exceptions-how-to-deal-with-exceptions-in-kotlin)