---
title:                "Das aktuelle Datum abrufen"
html_title:           "Clojure: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Die aktuelle Datum zu bekommen kann in vielen Anwendungen nützlich sein, zum Beispiel in der Buchhaltung, der Datensicherung oder in der Planung von Veranstaltungen. Das Ermitteln des aktuellen Datums in Clojure ist eine einfache und grundlegende Funktion, die jeder Entwickler kennen sollte.

## Wie geht's?

Das aktuelle Datum in Clojure kann mit der Funktion `now` aus dem `java.util.Date` Paket abgerufen werden. Diese Funktion gibt ein `Date` Objekt zurück, das Jahr, Monat, Tag, Stunden, Minuten, Sekunden und Millisekunden des aktuellen Zeitpunkts enthält.

```Clojure
(def today (java.util.Date/now))

; Ausgabe: #inst "2021-08-25T03:50:20.894-00:00"
```

Um das Datum in einem bestimmten Format auszugeben, kann die Funktion `format` aus dem `java.text.SimpleDateFormat` Paket verwendet werden. Hier ein Beispiel, das das heutige Datum im Format "DD.MM.YY" ausgibt:

```Clojure
(def formatter (java.text.SimpleDateFormat. "DD.MM.YY"))
(def formatted-date (formatter format today))

; Ausgabe: "25.08.21"
```

## Tiefer eintauchen

Das `Date` Objekt, das von der Funktion `now` zurückgegeben wird, ist ein spezielles Clojure-Wrapper-Objekt für Java-Datumsobjekte. Es ermöglicht den Zugriff auf alle Java-Datumsmethoden und -funktionen.

Zusätzlich zur `now` Funktion gibt es auch die Funktionen `today` und `yesterday`, die das Datum zum aktuellen Zeitpunkt bzw. einen Tag vor dem aktuellen Datum zurückgeben.

## Sieh auch

- [Dokumentation für die `now` Funktion](https://clojuredocs.org/clojure.java.api/java.util.Date/now)
- [Weitere Informationen zum `java.util.Date` Paket](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Beispiele zur Formatierung von Datum und Uhrzeit in Clojure](https://clojure.org/guides/faq#_formatting_dates_and_times)