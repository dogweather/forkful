---
title:    "Java: Das aktuelle Datum erhalten"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums ist eine häufige Aufgabe in der Programmierung. Es ermöglicht uns, den Zeitpunkt der Ausführung von Code zu verfolgen oder bestimmte Aktionen basierend auf dem aktuellen Datum auszuführen.

## Wie geht es

Die beste Möglichkeit, das aktuelle Datum in Java abzurufen, ist die Verwendung der Klasse `LocalDate` aus dem Paket `java.time`. Dies stellt sicher, dass wir mit dem Datum und der Zeitzone korrekt umgehen und mögliche Fehler vermeiden.

```Java
import java.time.LocalDate;

public class CurrentDateExample {

    public static void main(String[] args) {
    
        // Abrufen des aktuellen Datums
        LocalDate currentDate = LocalDate.now();
        
        // Ausgabe des Datums im Standardformat
        System.out.println("Heute ist " + currentDate);
    
    }
}
```

Der obige Code verwendet die Methode `now()` von `LocalDate`, um das aktuelle Datum zu erhalten. Mit der Methode `print()` können wir dann das Datum auf der Konsole ausgeben.

Die Ausgabe des obigen Beispiels wäre: `Heute ist 2021-05-20`.

Es ist auch möglich, das Datum in einem bestimmten Format auszugeben. Zum Beispiel:

```Java
// Ausgabe im Format dd/MM/yyyy
System.out.println("Heute ist " + currentDate.format(DateTimeFormatter.ofPattern("dd/MM/yyyy")));

// Ausgabe im Format MMM dd, yyyy
System.out.println("Heute ist " + currentDate.format(DateTimeFormatter.ofPattern("MMM dd, yyyy")));
```

Die Ausgabe für das erste Beispiel wäre `Heute ist 20/05/2021` und die zweite `Heute ist May 20, 2021`.

## Tiefer tauchen

Neben der Verwendung von `LocalDate` gibt es auch andere Möglichkeiten, das aktuelle Datum in Java abzurufen. Dazu gehören die Klassen `Calendar` und `Date`, die jedoch veraltet sind und nicht empfohlen werden, da sie nicht thread-safe sind und mögliche Probleme mit der Zeitzone verursachen können.

Es ist auch wichtig zu beachten, dass das Datum in Java eine Instanz der Klasse `LocalDate` ist, die nur das Datum selbst darstellt und keine Zeit oder Zeitzone enthält. Wenn Sie also das aktuelle Datum und die Uhrzeit benötigen, sollten Sie `LocalDateTime` oder `ZonedDateTime` verwenden.

## Siehe auch

- `java.time.LocalDate` [Java-Dokumentation] (https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- "Handling Date and Time" [Java Tutorials] (https://docs.oracle.com/javase/tutorial/datetime/index.html)
- "How to Get Current Date and Time in Java" [Baeldung-Artikel] (https://www.baeldung.com/java-get-current-date-time)