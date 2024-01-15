---
title:                "Das aktuelle Datum abrufen."
html_title:           "Java: Das aktuelle Datum abrufen."
simple_title:         "Das aktuelle Datum abrufen."
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das aktuelle Datum zu erhalten, ist in der Java-Programmierung eine häufige Aufgabe. Es ist wichtig, um bestimmte Funktionen in Anwendungen zu implementieren, die sich auf das Datum beziehen. Zum Beispiel können wir damit Geburtstage berechnen, Fristen einhalten und vieles mehr.

## Wie man das aktuelle Datum in Java erhält

Um das aktuelle Datum in Java zu erhalten, benötigen wir die Klasse "LocalDate" aus dem Paket "java.time". Mit dieser Klasse können wir eine Instanz des aktuellen Datums erstellen und es dann verwenden.

```Java
import java.time.LocalDate;  // Import der benötigten Klasse

public class CurrentDate {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now(); // Erstellung der aktuellen Datum-Instanz
        System.out.println("Das aktuelle Datum ist: " + currentDate); // Ausgabe des aktuellen Datums
    }
}
```

Die Ausgabe wird wie folgt sein:

```Java
Das aktuelle Datum ist: 2021-06-27
```

## Tiefere Einblicke

Es gibt viele Methoden, die wir mit der Klasse "LocalDate" verwenden können, um das aktuelle Datum zu bearbeiten oder Informationen darüber zu erhalten. Einige Beispiele sind:

- Um das aktuelle Jahr zu erhalten, können wir die Methode "getYear()" verwenden.

```Java
int year = currentDate.getYear();
System.out.println("Das aktuelle Jahr ist: " + year);
```

Die Ausgabe wird sein: 

```Java
Das aktuelle Jahr ist: 2021
```

- Wir können auch das aktuelle Datum mit anderen Datumsobjekten vergleichen, um zum Beispiel zu überprüfen, welches davon in der Zukunft liegt. Dazu können wir die Methode "isAfter()" verwenden.

```Java
LocalDate futureDate = LocalDate.of(2022, 1, 1); // Erstellung eines zukünftigen Datums
if (futureDate.isAfter(currentDate)) {  // Vergleich mit dem aktuellen Datum
    System.out.println("Das zukünftige Datum ist nach dem heutigen Datum.");
}
```

Die Ausgabe wird sein:

```Java
Das zukünftige Datum ist nach dem heutigen Datum.
```

Weitere Informationen über die Klasse "LocalDate" und ihre Methoden findest du in der offiziellen Java-Dokumentation (siehe "Siehe auch"-Abschnitt).

## Siehe auch

- [Java-Dokumentation für die Klasse "LocalDate"](https://docs.oracle.com/javase/10/docs/api/java/time/LocalDate.html)
- [Java-Tutorial: Arbeiten mit Datums- und Zeitangaben](https://docs.oracle.com/javase/tutorial/datetime/overview/index.html)
- [Ein Tutorial zu java.time](https://www.baeldung.com/java-8-date-time-intro)