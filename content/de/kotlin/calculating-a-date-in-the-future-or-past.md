---
title:    "Kotlin: Eine Datumskalkulation in der Zukunft oder Vergangenheit"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum
Das Berechnen eines Datums in der Zukunft oder Vergangenheit kann für verschiedene Anwendungen nützlich sein, wie z.B. die Erstellung von Kalendern oder die Planung von bestimmten Ereignissen. Mit Kotlin ist dies eine einfache Möglichkeit, die in diesem Artikel erläutert wird.

## Wie geht's?
Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, müssen wir zuerst das aktuelle Datum erhalten. Dies kann mit der `LocalDate.now()` Methode erreicht werden. Hier ist ein Beispiel:

```Kotlin
val currentDate = LocalDate.now()
```

Als nächstes müssen wir die gewünschte Anzahl an Tagen zur aktuellen Datum hinzufügen oder subtrahieren. Dies kann mit der `plusDays()` oder `minusDays()` Methode erfolgen. Hier ist ein Beispiel, um 7 Tage zur aktuellen Datum hinzuzufügen:

```Kotlin
val futureDate = currentDate.plusDays(7)
```

Die `futureDate` Variable enthält jetzt das Datum, das 7 Tage in der Zukunft liegt. Um ein Datum in der Vergangenheit zu berechnen, können wir dieselbe Methode verwenden, aber anstelle von `plusDays()` verwenden wir `minusDays()`. Hier ist ein Beispiel, um 3 Tage von der aktuellen Datum abzuziehen:

```Kotlin
val pastDate = currentDate.minusDays(3)
```

Das war's! Sie haben jetzt erfolgreich ein Datum in der Zukunft oder Vergangenheit berechnet.

## Tiefer eintauchen
Es gibt noch viele weitere Methoden, die wir in Kotlin verwenden können, um Datumsberechnungen durchzuführen. Zum Beispiel können wir mit `plusWeeks()` oder `plusMonths()` auch Wochen oder Monate zu einem Datum hinzufügen. Mit `format()` können wir auch das berechnete Datum in einem bestimmten Format anzeigen lassen. Hier ist ein Beispiel, um das Datum im Format "dd.MM.yyyy" auszugeben:

```Kotlin
val formattedDate = futureDate.format(DateTimeFormatter.ofPattern("dd.MM.yyyy"))
```

Es gibt auch viele andere Möglichkeiten, um Datumsberechnungen in Kotlin durchzuführen. Durchstöbern Sie die Kotlin-Dokumentation, um mehr darüber zu erfahren. 

## Siehe auch
- [Kotlin Dokumentation](https://kotlinlang.org/docs/datetime.html)
- [Java 8 Datums- und Uhrzeit-API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Parsing von Datum in Kotlin](https://www.baeldung.com/kotlin-datetime-parsing)