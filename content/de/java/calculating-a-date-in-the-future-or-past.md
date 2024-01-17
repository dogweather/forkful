---
title:                "Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Java: Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen eines Datums in der Zukunft oder Vergangenheit bezieht sich auf die Fähigkeit, ein Datum basierend auf einem gegebenen Datum und einer angegebenen Anzahl von Tagen, Monaten oder Jahren zu erhalten. Diese Funktion ist häufig für Anwendungen wie Kalender oder Planungstools erforderlich. Programmierer verwenden diese Methode, um effizient und genau Daten zu berechnen, ohne manuell jedes Datum einzugeben zu müssen.

## Wie funktioniert es:
In Java gibt es die Klasse `Calendar`, die verschiedene Methoden enthält, um ein Datum in der Zukunft oder Vergangenheit zu berechnen. Hier ist ein Beispiel, um das Datum in 30 Tagen zu berechnen:
```
Java Calendar cal = Calendar.getInstance();
cal.add(Calendar.DAY_OF_MONTH, 30);
System.out.println("Datum in 30 Tagen: " + cal.getTime());
```
Dieser Code erstellt ein `Calendar` Objekt mit dem aktuellen Datum und fügt dann 30 Tage hinzu. Das Ergebnis wird als `Date` Objekt zurückgegeben und ausgegeben.

Für komplexere Berechnungen können auch andere Methoden wie `add(Calendar.MONTH, int)` oder `add(Calendar.YEAR, int)` verwendet werden, die jeweils die Anzahl der Monate oder Jahre zu einem Datum hinzufügen.

## Tiefer Einblick:
Das Berechnen von Datumswerten geht auf die Einführung des gregorianischen Kalenders im Jahr 1582 zurück. Zuvor wurden verschiedene Kalender verwendet, die zu unterschiedlichen Berechnungen führen konnten. Heutzutage gibt es auch andere Möglichkeiten, Daten zu berechnen, wie zum Beispiel mit der `LocalDateTime` Klasse.

Um die Genauigkeit zu gewährleisten, verwendet das `Calendar` Objekt die Zeitzone des Systems, auf dem es ausgeführt wird. Dies kann zu Datenunterschieden führen, wenn sich die Zeitzone ändert oder wenn Programme auf verschiedenen Systemen ausgeführt werden.

## Siehe auch:
- Oracle Java Dokumentation: https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html
- Vergleich zwischen `Calendar` und `LocalDateTime`: https://www.baeldung.com/java-8-date-time-intro