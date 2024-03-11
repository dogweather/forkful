---
date: 2024-01-20 17:31:15.687933-07:00
description: "Das Berechnen eines zuk\xFCnftigen oder vergangenen Datums erlaubt es\
  \ uns, Zeitabst\xE4nde zu handhaben. Programmierer nutzen diese Funktion f\xFCr\
  \ Erinnerungen,\u2026"
lastmod: '2024-03-11T00:14:27.668952-06:00'
model: gpt-4-1106-preview
summary: "Das Berechnen eines zuk\xFCnftigen oder vergangenen Datums erlaubt es uns,\
  \ Zeitabst\xE4nde zu handhaben. Programmierer nutzen diese Funktion f\xFCr Erinnerungen,\u2026"
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
---

{{< edit_this_page >}}

## Was & Warum?

Das Berechnen eines zukünftigen oder vergangenen Datums erlaubt es uns, Zeitabstände zu handhaben. Programmierer nutzen diese Funktion für Erinnerungen, Planungen oder Zeitvergleiche.

## Anleitung:

```java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class DateCalculator {

    public static void main(String[] args) {
        LocalDate heute = LocalDate.now();
        LocalDate zukunft = heute.plusWeeks(2);
        LocalDate vergangenheit = heute.minusMonths(3);

        System.out.println("Heute: " + heute);
        System.out.println("Datum in 2 Wochen: " + zukunft);
        System.out.println("Datum vor 3 Monaten: " + vergangenheit);
    }
}
```

Beispiel-Ausgabe:

```
Heute: 2023-04-14
Datum in 2 Wochen: 2023-04-28
Datum vor 3 Monaten: 2023-01-14
```

## Tiefgang:

Historisch gesehen wurde das Datum-Handling in Java mit `java.util.Date` und `java.util.Calendar` erledigt, was nicht ohne Tücken war. `java.time` - auch bekannt als JSR-310 - löst viele dieser Probleme und ist seit Java 8 Standard.

Abseits der Standardbibliothek gibt es Libraries wie Joda-Time, die heute weitgehend durch `java.time` ersetzt wurden.

Zu den Details: `LocalDate` ist unveränderlich und thread-safe. Methoden wie `plus` und `minus` erstellen jeweils neue Instanzen. Sie vermeiden Probleme der mutierenden Zustandsveränderungen, die mit `java.util.Date` zu Kopfschmerzen führen konnten.

## Siehe auch:

- Oracle Tutorial zu `java.time`: https://docs.oracle.com/javase/tutorial/datetime/
- JSR-310 User Guide: https://www.oracle.com/technical-resources/articles/java/joda.html
- Java-Dokumentation für `LocalDate`: https://docs.oracle.com/javase/10/docs/api/java/time/LocalDate.html

Durch die Links bekommt ihr tiefere Einblicke in die Möglichkeiten von `java.time`.
