---
title:                "Ein Datum aus einer Zeichenkette analysieren"
html_title:           "Kotlin: Ein Datum aus einer Zeichenkette analysieren"
simple_title:         "Ein Datum aus einer Zeichenkette analysieren"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Parsing (oder auch Parstudiung) eines Datums aus einem String ist eine Methode, um ein Datum in einer bestimmten Form (z.B. "dd/MM/yyyy") aus einem Text zu extrahieren. Programmierer verwenden diese Methode, um benutzerdefinierte Eingaben oder Daten aus externen Quellen wie Datenbanken oder APIs zu verarbeiten.

## So geht's:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

// Erstelle ein Beispiel-Datum als String
val dateString = "12/03/2021"

// Definiere das gewünschte Datumsformat
val dateFormat = "dd/MM/yyyy"

// Parse das Datum aus dem String unter Verwendung des gewünschten Formats
val parsedDate = LocalDate.parse(dateString, DateTimeFormatter.ofPattern(dateFormat))

// Gib die geparste Date aus
println("Geparstes Datum: $parsedDate")
```

**Output:** Geparstes Datum: 2021-03-12

## Tiefere Einblicke:

- Historischer Kontext: Früher mussten Programmierer komplexe Algorithmen verwenden, um Daten aus Strings zu parsen. Mit der Einführung der Java-Klasse "DateTimeFormatter" und der Unterstützung für reguläre Ausdrücke wurde dieser Prozess viel einfacher.
- Alternativen: Neben der Verwendung von "DateTimeFormatter" gibt es auch andere Bibliotheken oder Frameworks, die das Parsen von Daten aus Strings unterstützen, wie z.B. "Joda-Time".
- Implementierungsdetails: Die Klasse "DateTimeFormatter" verwendet ein Muster-basiertes Ansatz, um das gewünschte Datumsformat zu definieren. Es kann auch verschiedene Optionen wie Lokalisierung oder Zeitzone anpassen.

## Siehe auch:

- [Offizielle Kotlin-Dokumentation für das Parsen von Datums aus Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/java.time.-date-time/parse.html)
- [Tutorial zur Verwendung von regulären Ausdrücken in Kotlin](https://www.baeldung.com/kotlin-regex)