---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Java Programmierung: So holen Sie das aktuelle Datum

## Was & Warum?
`date` und `time` sind Schlüsselkonzepte in der Programmierung. Sie sind unabdingbar, wenn man Logfiles erstellt, Zeitstempel setzt oder eine Zeiterfassung implementiert.

## So geht's:
Java bietet mehrere Möglichkeiten, das aktuelle Datum zu ermitteln. Ein gängiger Weg ist die Nutzung der Klasse `LocalDate`. Hier ein Codebeispiel dazu:

```Java
import java.time.LocalDate;

public class Main {
  public static void main(String[] args) {
    LocalDate currentDate = LocalDate.now();
    System.out.println("Heutiges Datum: " + currentDate);
  }
}
```
Beim Ausführen gibt es die Ausgabe:

```Java
Heutiges Datum: 2023-04-05
```

## Deep Dive
Früher hat man das aktuelle Datum oft mit `java.util.Date` ermittelt. Aber diese Klasse ist nicht sehr benutzerfreundlich und daher heute veraltet. Stattdessen wird nun `java.time.LocalDate` oder `java.time.LocalDateTime` empfohlen. 

Alternativ kann man auch die Klasse `java.util.Calendar` verwenden, aber `LocalDate` und `LocalDateTime` bieten viele Vorteile. Unter anderem ist die API intuitiver und es gibt mehr eingebaute Methoden, z.B. zum Formatieren des Datums.

Die Implementierungsdetails von `LocalDate.now()` sind auch interessant. Die Methode ruft intern `Clock.systemDefaultZone().instant()` auf. Das bedeutet, es wird die Systemzeitzone verwendet, um das aktuelle Datum zu ermitteln. 

## Siehe auch
Für weitere Informationen zu diesem Thema, schauen Sie sich bitte folgende Ressourcen an:
- [Oracle Java Documentation](https://docs.oracle.com/en/java/)
- [Baeldung Tutorials - Java Dates](https://www.baeldung.com/java-8-date-time-intro)
- [StackOverflow - Get current date in java](https://stackoverflow.com/questions/368094/get-current-date-in-java)