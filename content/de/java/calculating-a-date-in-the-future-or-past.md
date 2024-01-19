---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Java: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines zukünftigen oder vergangenen Datums ist eine häufig benötigte Aktion in der Programmierung, der sog. "Datumsumrechnung". Ob in Buchhaltungsprogrammen oder in Fitness-Apps, wir müssen häufig Daten in der Zukunft oder der Vergangenheit bestimmen.

## So geht's:
Java stellt uns mit der Klasse `java.time.LocalDate` ein mächtiges Werkzeug zur Verfügung. Zum Beispiel:

```Java
import java.time.LocalDate;

public class FutureDate {
    public static void main(String[] args){
        LocalDate today = LocalDate.now();
        LocalDate futureDate = today.plusDays(5);
        System.out.println("Das Datum in fünf Tagen ist: " + futureDate);
    }
}
```
Wird obiges Code-Schnipsel ausgeführt, erhalten wir folgende Ausgabe:

```
Das Datum in fünf Tagen ist: 2022-08-31
```
Ebenso einfach lässt sich ein Datum in der Vergangenheit berechnen, indem man die Methode `minusDays(int days)` anstatt `plusDays(int days)` verwendet.

## Deep Dive
Historisch gesehen war die Arbeit mit Datumsangaben in Java vor der Einführung von `LocalDate` in Java 8 eine holprige Angelegenheit - die `java.util.Date`- und `java.util.Calendar`-Klassen waren unhandlich und fehleranfällig.

Eine mögliche Alternative zu `LocalDate` ist Joda-Time, eine Drittanbieter-Bibliothek, die ähnliche Funktionalitäten bereitstellt. Allerdings bevorzugen die meisten Java-Entwickler `LocalDate` wegen seiner Einfachheit und direkten Integration in die Java Standard-Bibliothek.

## Siehe Auch
Weiterführende Informationen und alternative Möglichkeiten zur Datumsumrechnung finden Sie unter folgenden Links:

- [Java LocalDate Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Arbeiten mit Zeit und Datum in Java](https://www.baeldung.com/java-8-date-time-intro)
- [Joda-Time Dokumentation](https://www.joda.org/joda-time/)