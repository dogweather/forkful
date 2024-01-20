---
title:                "Schreiben auf Standardfehler"
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben auf Standard Error (`stderr`) ermöglicht es, Fehlermeldungen von normaler Ausgabe (`stdout`) zu trennen. So können Entwickler Probleme leichter erkennen und behandeln.

## How to:
```kotlin
fun main() {
    System.err.println("Das ist eine Fehlermeldung.")
    println("Das ist normale Ausgabe.")
}
```

Ausgabe:
```
Das ist eine Fehlermeldung.
Das ist normale Ausgabe.
```

Fehler und normale Nachrichten lassen sich umleiten und separat behandeln.

## Deep Dive
Historisch stammen `stdout` und `stderr` aus der Unix-Welt, um unterschiedliche Datenströme zu behandeln. Alternativ zu `System.err` gibt es in Java-basierten Sprachen wie Kotlin Logging-Frameworks, die flexibler sind. `System.err` ist direkt an die JVM gekoppelt und kann ohne externe Bibliotheken genutzt werden.

## See Also
- [Kotlin Documentation](https://kotlinlang.org/docs/reference/)
- [Java™ Platform Standard Ed. 8 (System)](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Baeldung on Logging in Kotlin](https://www.baeldung.com/kotlin/logging)