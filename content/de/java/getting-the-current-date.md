---
title:                "Java: Die aktuelle Datum abrufen"
simple_title:         "Die aktuelle Datum abrufen"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das aktuelle Datum ist eine wichtige Information in vielen Java-Programmen, da es oft für die Verarbeitung von Daten und die Erstellung von Berichten benötigt wird.

## Wie

Um das aktuelle Datum in Java zu bekommen, müssen wir die ```LocalDate``` Klasse aus dem ```java.time``` Paket importieren. Anschließend können wir die Methode ```now()``` aufrufen, um das aktuelle Datum zu erhalten.

```Java
import java.time.LocalDate;

// ...

LocalDate currentDate = LocalDate.now();
System.out.println(currentDate);
```

Die Ausgabe dieses Codes wird das aktuelle Datum im Format "YYYY-MM-DD" sein, abhängig von der aktuellen Zeitzone. Wir können auch spezifische Teile des Datums mit den entsprechenden Methoden der ```LocalDate``` Klasse abrufen, wie zum Beispiel den Monat oder das Jahr.

## Deep Dive

Hinter den Kulissen benutzt die ```LocalDate``` Klasse das sogenannte "Unix Epoch" System, bei dem das Datum als Anzahl von Sekunden seit dem 1. Januar 1970 gespeichert wird. Dies ermöglicht es, das Datum unabhängig von der Zeitzone oder dem Kalender zu speichern und darzustellen.

Es ist auch wichtig zu beachten, dass die Methode ```now()``` das Datum des Systems verwendet, auf dem das Programm ausgeführt wird. Wenn dies ein anderer Rechner ist als der, auf dem das Programm entwickelt wurde, können sich Unterschiede im Datum ergeben.

## Siehe auch

- [Oracle Dokumentation zur LocalDate Klasse](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Weitere Informationen zur Unix Epoch Zeit](https://www.epochconverter.com/)