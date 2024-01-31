---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:14:50.316266-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"

category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums in Java bedeutet einfach, den heutigen Tag aus dem System zu holen. Programmierer machen das häufig, um Zeitstempel zu erzeugen, Berichte zu datieren, oder um Funktionen zu planen, die auf der Zeit basieren.

## How to:
Den aktuellen Tag in Java zu bekommen ist simpel. Hier sind ein paar Beispiele, die das LocalDate und das Calendar-Objekt von Java verwenden:

```java
import java.time.LocalDate;
import java.util.Calendar;

public class CurrentDateExample {
    public static void main(String[] args) {
        // Mit LocalDate
        LocalDate today = LocalDate.now();
        System.out.println("Heute ist: " + today);

        // Mit Calendar
        Calendar cal = Calendar.getInstance();
        System.out.println("Heute ist: " + cal.getTime());
    }
}
```

Wenn du das ausführst, könntest du so etwas sehen:

```
Heute ist: 2023-04-02
Heute ist: Sun Apr 02 15:20:34 CEST 2023
```

## Deep Dive:
`LocalDate` ist Teil des modernen Java Date & Time API, das in Java 8 eingeführt wurde, um die Datums- und Zeitfunktionalität besser zu verwalten. Vor Java 8 benutzten Programmierer hauptsächlich das `Date` und `Calendar`-Objekt, obwohl diese nicht ohne Probleme waren – sie waren nicht unveränderlich und hatten weniger intuitive Methoden.

Alternativ können Entwickler auch `java.time.Instant` nutzen, um ein Datum mit Zeitstempel im Unix-Format zu bekommen, das mit der Epoche (1. Januar 1970, 00:00:00 GMT) beginnt.

Was die Implementierung betrifft, liefert `LocalDate.now()` das heutige Datum bezogen auf die Systemuhr und die Standardzeitzone. Das `Calendar` ist älter und davor brauchten wir `getInstance()` um eine `Calendar`-Instanz zu bekommen, die etwas komplizierter im Umgang ist.

## See Also:
- Die offizielle Java-Dokumentation zur `LocalDate`-Klasse: [Oracle Docs: LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- Infos über die `Calendar`-Klasse: [Oracle Docs: Calendar](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- Mehr zum umfassenden Java Date and Time API: [Oracle Tutorials - Date Time](https://docs.oracle.com/javase/tutorial/datetime/)
