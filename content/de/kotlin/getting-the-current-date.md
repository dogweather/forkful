---
title:                "Das aktuelle Datum erhalten"
html_title:           "Kotlin: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

#Warum

Das aktuelle Datum ist ein wichtiger Bestandteil der meisten Anwendungen und kann in verschiedenen Szenarien nützlich sein, wie zum Beispiel bei der Datenspeicherung oder der Anzeige von aktualisierten Informationen für den Benutzer.

#Wie geht es

Die aktuelle Datum in Kotlin zu erhalten ist einfach und erfordert nur eine kurze Codezeile. Wir benutzen die Methode `LocalDate.now()` um die aktuelle Datum zu erhalten und verwenden das optionale Argument `ZoneId` um die Zeitzone zu spezifizieren.

```Kotlin
val currentDate = LocalDate.now(ZoneId.of("Europe/Berlin"))
```

Um die aktuelle Datum in einem lesbaren Format auszugeben, können wir die Methode `format()` verwenden und ein passendes Muster für das gewünschte Datumformat angeben.

```Kotlin
val dateString = currentDate.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"))
println("Das aktuelle Datum ist: $dateString")
```

Die Ausgabe könnte dann folgendermaßen aussehen:

```
Das aktuelle Datum ist: 06.07.2021
```

#Deep Dive

In Kotlin gibt es verschiedene Datums- und Zeittypen, die die Arbeit mit Datum und Zeit erleichtern. `LocalDate` repräsentiert nur ein Datum ohne Zeitinformationen und ist die beste Wahl, wenn wir nur das aktuelle Datum benötigen. Wenn jedoch auch die Zeit relevant ist, können wir `LocalDateTime` oder `ZonedDateTime` verwenden, die beide das Datum und die Uhrzeit in einem Objekt speichern.

Außerdem bietet Kotlin auch die Möglichkeit, mit Zeitzonen zu arbeiten, indem wir `ZoneId` verwenden. Diese Klasse ermöglicht es uns, eindeutige Zeitzonen zu identifizieren und zu verwenden, anstatt uns auf die Standard-System-Zeitzone zu verlassen.

#Siehe auch

- [Offizielle Kotlin Dokumentation zu Datum und Zeit](https://kotlinlang.org/docs/datetime.html)
- [Einführung in die Programmierung mit Kotlin](https://www.udemy.com/java-kotlin-programmierung/)