---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Drucken von Debug-Ausgaben ist im Grunde eine Methode, bei der Programmierer Text auf der Konsole ausgeben, um Programminformationen während der Laufzeit zu verfolgen. Es wird gemacht, um Fehler im Code zu identifizieren und zu beheben.

## So geht’s:
In Kotlin kann die `println()` Funktion verwendet werden, um Debug-Ausgaben anzuzeigen. Sieh dir unser einfaches Beispiel an:

```Kotlin
fun main() {
    val message = "Hallo, Kotlin!"
    println("Debug-Ausgabe: $message")
}
```
Die Ausgabe wäre dann:
```
Debug-Ausgabe: Hallo, Kotlin!
```

## Tiefgang:
Historisch gesehen ist das Drucken von Debug-Ausgaben eine der ältesten Methoden zur Fehlerbehebung. Es ist einfach, es erfordert keine zusätzlichen Tools und ist ziemlich universell.

Alternativ können in Kotlin, sowie in den meisten modernen Sprachen, Debugger oder Logging-Frameworks eingesetzt werden, die flexiblere und robustere Möglichkeiten zur Fehlerbehebung bieten.

Die `println()` Funktion in Kotlin implementiert ihre Aufgabe, indem sie den `System.out.println()` Aufruf aus Java einhüllt, was wiederum die native Printstream-Methode des Betriebssystems verwendet.

## Siehe auch:
1. [Offizielle Kotlin-Dokumentation](https://kotlinlang.org/docs/home.html)
2. [Einführung in das Logging mit Kotlin](https://www.loggly.com/blog/introduction-logging-kotlin/)
3. [Java-Debugging mit IntelliJ IDEA](https://www.jetbrains.com/de-de/idea/guide/tutorials/debugging-a-java-application/)