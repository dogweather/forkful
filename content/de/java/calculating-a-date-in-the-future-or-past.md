---
title:    "Java: Berechnen eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datumsangaben in der Zukunft oder Vergangenheit kann in vielen Situationen nützlich sein, z.B. um Fristen einzuhalten oder um Erinnerungen zu setzen. In diesem Blog-Beitrag zeigen wir Ihnen, wie Sie mit Java programmatisch ein Datum in der Zukunft oder Vergangenheit berechnen können.

## Wie es geht

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, verwenden wir die Klasse `Calendar` aus dem Paket `java.util`. Diese Klasse bietet verschiedene Methoden, um Datumsangaben zu manipulieren. Wir werden uns auf die Methode `add()` konzentrieren, die es uns ermöglicht, eine bestimmte Zeiteinheit (z.B. Tage, Monate) zu einem gegebenen Datum hinzuzufügen oder davon abzuziehen.

Wir werden zunächst ein `Calendar`-Objekt erstellen und es auf das aktuelle Datum setzen:

```Java
Calendar calendar = Calendar.getInstance();
```

Nun können wir mit der `add()`-Methode eine bestimmte Anzahl von Tagen zu diesem Datum hinzufügen oder davon abziehen. Wir verwenden dazu die Konstanten `Calendar.DATE` und `Calendar.MONTH`, um anzugeben, welche Zeiteinheit wir manipulieren wollen:

```Java
calendar.add(Calendar.DATE, 7); // Fügt 7 Tage hinzu
calendar.add(Calendar.MONTH, -3); // Zieht 3 Monate ab
```

Nachdem wir die Datumsberechnung durchgeführt haben, können wir das Ergebnis in einem bestimmten Format ausgeben, z.B. als `String`:

```Java
// Formatierung des Datums als "Tag.Monat.Jahr"
SimpleDateFormat format = new SimpleDateFormat("dd.MM.yyyy");
String futureDate = format.format(calendar.getTime());
System.out.println("Datum in der Zukunft: " + futureDate);
```

Die Ausgabe sieht dann z.B. so aus:

```Console
Datum in der Zukunft: 25.07.2021
```

## Tiefer Einblick

Die Klasse `Calendar` bietet noch viele weitere Methoden, um mit Datumsangaben zu arbeiten. Sie können z.B. verschiedene Kalender-Systeme verwenden (z.B. gregorianischer oder chinesischer Kalender), die Zeitzone anpassen oder bestimmte Feiertage berücksichtigen. Es gibt auch die Möglichkeit, benutzerdefinierte `Calendar`-Objekte zu erstellen und zu verwenden.

Es ist wichtig zu beachten, dass die Klasse `Calendar` veraltet ist und durch die neuere Klasse `LocalDate` aus dem Paket `java.time` ersetzt wurde. `LocalDate` bietet eine einfachere und präzisere Möglichkeit, mit Datumsangaben zu arbeiten. Wir empfehlen daher, `LocalDate` anstelle von `Calendar` zu verwenden.

## Siehe auch

- [Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java Tutorials: Date Time](https://docs.oracle.com/javase/tutorial/datetime/index.html)
- [Baeldung: How to Calculate Date/Time Difference in Java](https://www.baeldung.com/java-date-difference)