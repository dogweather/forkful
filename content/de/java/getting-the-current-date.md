---
title:                "Java: Das aktuelle Datum abrufen"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das heutige Datum kann in vielen Anwendungsfällen nützlich sein. Zum Beispiel können Sie es verwenden, um die Zeitstempel für bestimmte Aktionen zu speichern oder um die Ausführung von bestimmten Tasks automatisiert zu planen. Es ist daher wichtig zu wissen, wie man in Java das aktuelle Datum abruft.

## Wie geht das

Um das aktuelle Datum in Java zu erhalten, können Sie die Klasse `LocalDate` aus dem Paket `java.time` verwenden. Hier ist ein Beispielcode:

```Java
import java.time.LocalDate;

public class DatumAbrufen {

    public static void main(String[] args) {
    
        LocalDate heute = LocalDate.now();
        System.out.println("Heute ist der " + heute);
    }
}
```

**Ausgabe:**

```
Heute ist der 28. September 2021
```

In diesem Beispiel importieren wir die Klasse `LocalDate` und erstellen dann eine Instanz davon mit `LocalDate.now()`, um das heutige Datum abzurufen. Anschließend geben wir das Datum mit `System.out.println()` aus.

Sie können auch das aktuelle Datum und die Zeit mit der Klasse `LocalDateTime` abrufen. Hier ist ein Beispielcode:

```Java
import java.time.LocalDateTime;

public class DatumUndZeitAbrufen {

    public static void main(String[] args) {
    
        LocalDateTime jetzt = LocalDateTime.now();
        System.out.println("Aktuelles Datum und Uhrzeit: " + jetzt);
    }
}
```

**Ausgabe:**

```
Aktuelles Datum und Uhrzeit: 2021-09-28T13:30:15.179
```

Wie Sie sehen können, enthält die Ausgabe auch die Uhrzeit. Sie können außerdem die Methode `now()` von anderen Klassen wie `LocalTime` und `ZonedDateTime` verwenden, um nur die Uhrzeit oder Datum und Uhrzeit zu erhalten.

## Tief eintauchen

Um das aktuelle Datum abzurufen, muss Ihre Anwendung wissen, welche Zeitzone sie verwenden soll. Standardmäßig wird die Systemzeitzone verwendet, aber Sie können diese ändern, indem Sie die Methode `atZone()` verwenden und eine andere Zeitzone angeben.

```Java
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;

public class DatumInAndererZeitzoneAbrufen {

    public static void main(String[] args) {
    
        LocalDate heute = LocalDate.now();
        System.out.println("Heute ist der " + heute);
        
        ZonedDateTime datumInNewYork = heute.atZone(ZoneId.of("America/New_York"));
        System.out.println("Heute in New York: " + datumInNewYork);
    }
}
```

**Ausgabe:**

```
Heute ist der 28. September 2021
Heute in New York: 2021-09-28T00:00:00-04:00[America/New_York]
```

Sie können auch das aktuelle Datum zu einem bestimmten Zeitpunkt in der Zukunft oder Vergangenheit erhalten, indem Sie die Methode `plus()` verwenden. Sie können auch die Methode `minus()` verwenden, um ein Datum in der Vergangenheit zu erhalten.

```Java
import java.time.LocalDate;

public class DatumInDerZukunftAbrufen {

    public static void main(String[] args) {
    
        LocalDate heute = LocalDate.now();
        System.out.println("Heute ist der " + heute);
        
        LocalDate inEinerWoche = heute.plusWeeks(1);
        System.out.println("In einer Woche: " + inEinerWoche);
        
        LocalDate inEinemMonat = heute.plusMonths(1);
        System.out.println("In einem Monat: " + inEinemMonat);
    }
}
```

**Ausgabe:**

```
Heute ist der 28. September 2021
In einer Woche: 2021-10-05
In einem Monat: 2021-10-28
```

Diese Methoden werden auch nützlich sein, wenn Sie ein Datum für die Planung von Aufgaben in Ihrer Anwendung benötigen.

## Siehe auch

- [Oracle Dokumentation: java.time Package](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Baeldung: Manipulating dates in Java with LocalDate, LocalDateTime and ZonedDateTime](https://www.baeldung.com/java-date-manipulation)