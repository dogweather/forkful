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

## Was & Warum?
Die aktuelle Datumsermittlung in der Programmierung bezieht sich auf das Abrufen des aktuellen Datums auf dem Computer oder Gerät, an dem das Programm ausgeführt wird. Programmierer tun dies, um das aktuelle Datum für verschiedene Zwecke zu verwenden, wie z.B. zur Anzeige in einer Benutzeroberfläche oder zur Berechnung von bestimmten Datumswerten.

## Wie geht's?
Die aktuelle Datumsermittlung kann in Kotlin auf verschiedene Weise erfolgen. Wenn Sie das aktuelle Datum als eine Zeichenfolge erhalten möchten, können Sie die folgende Funktion verwenden:

```
val currentDate = LocalDate.now()
```

Der Wert von `currentDate` wird dann dem heutigen Datum entsprechen.

Wenn Sie jedoch die genaue Uhrzeit zusammen mit dem Datum erhalten möchten, können Sie die folgende Funktion verwenden:

```
val currentDateAndTime = LocalDateTime.now()
```

Der Wert von `currentDateAndTime` enthält sowohl das Datum als auch die Uhrzeit zum Zeitpunkt der Ausführung.

## Tiefere Einblicke
Die Möglichkeit, das aktuelle Datum in einem Programm zu ermitteln, ist von entscheidender Bedeutung, da es eine wichtige Rolle bei der Darstellung und Verarbeitung von Datumswerten spielt. Eine Alternative zur Verwendung der integrierten Funktionen in Kotlin besteht darin, ein benutzerdefiniertes Datum und eine Uhrzeit-Klasse zu erstellen, um das aktuelle Datum zu verfolgen. In Bezug auf die Implementierung nutzt Kotlin die Funktionen der Java-Klasse `java.time`, um das aktuelle Datum zu erhalten.

## Siehe auch
- [Java Tutorial zu java.time](https://docs.oracle.com/javase/tutorial/datetime/iso/index.html)
- [Kotlin-Dokumentation zu Date and Time](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/index.html)