---
date: 2024-01-20 17:52:46.688350-07:00
description: "Debug-Ausgaben dienen dazu, den Programmfluss und Variablenwerte w\xE4\
  hrend der Laufzeit zu beobachten. Programmierer nutzen sie, um Fehler schneller\
  \ zu\u2026"
lastmod: '2024-03-13T22:44:53.848996-06:00'
model: gpt-4-1106-preview
summary: "Debug-Ausgaben dienen dazu, den Programmfluss und Variablenwerte w\xE4hrend\
  \ der Laufzeit zu beobachten. Programmierer nutzen sie, um Fehler schneller zu\u2026"
title: Debug-Ausgaben drucken
---

{{< edit_this_page >}}

## Was & Warum?
Debug-Ausgaben dienen dazu, den Programmfluss und Variablenwerte während der Laufzeit zu beobachten. Programmierer nutzen sie, um Fehler schneller zu finden und ihre Software zu verstehen.

## How to:
Debugging in Kotlin ist simpel. Verwende `println()` für schnelles Feedback:
```kotlin
fun main() {
    val debugMessage = "Das ist eine Debug-Nachricht."
    println(debugMessage)
}
```
Ausgabe:
```
Das ist eine Debug-Nachricht.
```

Für formatierte Ausgaben nutze `String`-Templates:
```kotlin
fun main() {
    val temperatur = 22.5
    println("Die aktuelle Temperatur beträgt: $temperatur Grad Celsius.")
}
```
Ausgabe:
```
Die aktuelle Temperatur beträgt: 22.5 Grad Celsius.
```

## Deep Dive
In den Anfängen bestanden Debugging-Methoden oft aus einfachem Textausgaben, die in die Konsole geschrieben wurden. Moderne IDEs bieten nun komplexere Tools wie Breakpoints und Watchers, trotzdem bleibt `println()` beliebt wegen seiner Einfachheit. Alternativ zu `println()` gibt es in Kotlin noch `print()`, `error()`, `assert()` und Logging-Frameworks wie `log4j` oder `SLF4J`, welche konfigurierbare Loglevels bereitstellen (INFO, ERROR, DEBUG usw.). Die Wahl des Tools hängt oft vom Kontext und der Komplexität des Projekts ab. So wird in Produktionscodes eher auf Logging-Frameworks zurückgegriffen, während schnelle Debugging-Tasks oft mit `println()` erledigt werden.

## See Also
- [Kotlin Dokumentation](https://kotlinlang.org/docs/reference/)
- [SLF4J Logging Framework](http://www.slf4j.org/)
- [log4j Logging Framework](https://logging.apache.org/log4j/2.x/)
