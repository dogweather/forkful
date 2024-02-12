---
title:                "Protokollierung"
aliases:
- /de/kotlin/logging/
date:                  2024-01-26T01:06:49.588013-07:00
model:                 gpt-4-1106-preview
simple_title:         "Protokollierung"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/logging.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Protokollieren ist im Grunde die Praxis, Ereignisse und Daten aus einer Softwareanwendung in eine externe Ausgabe, wie eine Datei oder Konsole, aufzuzeichnen. Programmierer protokollieren Vorgänge, um durch den Code zu navigieren, Probleme zu beheben und das Verhalten einer App in freier Wildbahn im Auge zu behalten, und bieten kritische Einsichten, die auf keine andere Weise so effektiv gewonnen werden können.

## Wie geht das:

In Kotlin kann das Protokollieren mithilfe der eingebauten `println()`-Funktion für einfache Fälle erfolgen oder mit ausgefeilteren Bibliotheken wie SLF4J mit Logback oder Log4j für fortgeschrittene Bedürfnisse.

Hier ist ein einfaches Beispiel mit `println()`:

```Kotlin
fun main() {
    println("Einfache Protokollnachricht: Anwendung gestartet.")
    // ... etwas Anwendungslogik hier ...
    try {
        // Einen Fehler simulieren
        throw Exception("Simulierter Fehler")
    } catch (e: Exception) {
        println("Fehlerprotokollnachricht: " + e.message)
    }
}
```

Ausgabe:
```
Einfache Protokollnachricht: Anwendung gestartet.
Fehlerprotokollnachricht: Simulierter Fehler
```

Und hier ein Snippet mit SLF4J konfiguriert mit Logback:

```Kotlin
import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger("MyAppLogger")

fun main() {
    logger.info("Strukturierte Protokollnachricht: App gestartet.")
    // ... etwas Anwendungslogik hier ...
    try {
        // Einen Fehler simulieren
        throw Exception("Simulierter Fehler")
    } catch (e: Exception) {
        logger.error("Strukturiertes Fehlerprotokoll: ", e)
    }
}
```

Bei entsprechender Logback-Konfiguration würde die Ausgabe formatiert sein und könnte so aussehen, wenn sie in eine Protokolldatei geschrieben wird:
```
[INFO] - 2023-03-29 14:15:42 - MyAppLogger - Strukturierte Protokollnachricht: App gestartet.
[ERROR] - 2023-03-29 14:15:43 - MyAppLogger - Strukturiertes Fehlerprotokoll: 
java.lang.Exception: Simulierter Fehler
   at com.myapp.Main.main(Main.kt:10)
```

## Tiefergehende Betrachtung

Historisch gesehen entwickelte sich die Protokollierung in der Software parallel zur steigenden Komplexität von Anwendungen und Systemen. Einfache Druckbefehle waren in den Anfangstagen ausreichend, in denen Programme oft von den Entwicklern selbst ausgeführt und debuggt wurden. Aber als Systeme vernetzt wurden und in verschiedenen Umgebungen unter unterschiedlichen Benutzern liefen, wurde ein robustes und beständiges Protokolliersystem entscheidend.

Bevor Kotlin populär wurde, hatten Java-Entwickler Bibliotheken wie Log4j und später SLF4J weit verbreitet angenommen. Diese haben ähnliche Praktiken in Kotlin inspiriert, die Interoperabilität von Kotlin mit Java-Bibliotheken nutzend. SLF4J fungiert als Abstraktionsschicht, die es erlaubt, die tatsächliche Protokollierungsimplementierung auszutauschen – normalerweise sind Logback oder Log4j2 die bevorzugten Optionen.

Kotlin ermöglicht auch plattformübergreifende Protokollierungslösungen, die über JVM, JavaScript und Native funktionieren, beispielsweise durch den `expect`/`actual`-Mechanismus, der plattformspezifische Implementierungen abstrahiert.

Im Gegensatz zu spezialisierten Protokollierungsbibliotheken bleibt println die einfachste Form der Protokollierung, da keine zusätzliche Einrichtung oder Abhängigkeiten erforderlich sind. Allerdings ist sie für Produktionsanwendungen normalerweise ungeeignet wegen ihrer Mangel an Funktionen wie Protokollebenen, Protokollrotation und strukturierten Formaten.

Andere häufige Funktionen von fortschrittlichen Protokollierungsframeworks umfassen:

- Protokollebenen (DEBUG, INFO, WARN, ERROR, usw.), um die Dringlichkeit von Protokollnachrichten zu kategorisieren.
- Ausgabe an verschiedene Ziele, wie Konsole, Datei, Datenbanken oder Netzwerkdienste.
- Automatische Protokollrotation und Aufbewahrungspolitik.
- Verteilte Nachverfolgungsunterstützung für Microservices-Architekturen.
- Strukturierte Protokollierung mit Formaten wie JSON, die gut mit Protokollanalysesystemen integrieren.

Diese Werkzeuge und Funktionen sind entscheidend für die Aufrechterhaltung eines zuverlässigen, beobachtbaren Systems, insbesondere in komplexen, verteilten oder hochskalierten Umgebungen.

## Siehe auch

Für weiteres Lernen und Einblicke in die Protokollierung mit Kotlin, schauen Sie sich an:

- SLF4J (Simple Logging Facade for Java) [http://www.slf4j.org/](http://www.slf4j.org/)
- Logback, der Nachfolger von Log4j [http://logback.qos.ch/](http://logback.qos.ch/)
- Log4j 2 [https://logging.apache.org/log4j/2.x/](https://logging.apache.org/log4j/2.x/)
- Kotlin Multiplattform-Dokumentation über 'expect' und 'actual' Deklarationen: [https://kotlinlang.org/docs/multiplatform.html](https://kotlinlang.org/docs/multiplatform.html)
- Ein Leitfaden zur strukturierten Protokollierung in Kotlin: [https://ktor.io/docs/logging.html](https://ktor.io/docs/logging.html)
