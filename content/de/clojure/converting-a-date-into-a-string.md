---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Clojure: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man ein Datum in einen String konvertieren möchte. Ein häufiger Grund ist, dass ein bestimmtes Dateiformat oder eine Datenbank dies erfordert. Indem man ein Datum in einen String umwandelt, kann man es auch in verschiedenen Bereichen des Codes einfacher verwenden.

## Wie man es macht

Die Umwandlung eines Datums in einen String kann in Clojure mithilfe der `format` Funktion durchgeführt werden. Diese Funktion nimmt zwei Argumente an: ein Datumsobjekt und ein String-Format. Das Datumsobjekt muss eine Instanz von `java.util.Date` sein. Das String-Format definiert, wie das Datum in einen String umgewandelt werden soll.

```Clojure
(def my-date (java.util.Date.)) ;; Erstelle ein neues Datumsobjekt

(format my-date "dd.MM.yyyy") ;; Ausgabe: "02.08.2019"
(format my-date "EEE, dd MMMM yyyy") ;; Ausgabe: "Fr, 02 August 2019"
```

In den obigen Beispielen können wir sehen, dass das Format argument verwendet wird, um das Datum in verschiedene Formate zu konvertieren. Einige häufig verwendete Formatierungen sind:

- `dd` - Tag des Monats (zweistellig)
- `MM` - Monat (zweistellig)
- `yyyy` - Jahr (vierstellig)
- `EEE` - Abgekürzter Wochentag (z.B. "Mo", "Di", "Mi")
- `MMMM` - Vollständiger Monatsname (z.B. "Januar", "Februar")

Es gibt noch viele weitere Formatierungsoptionen, die in der offiziellen Clojure-Dokumentation zu finden sind.

## Tiefer Einblick

Um das Datum in eine andere Zeitzone zu konvertieren, kann die `with-time-zone` Funktion verwendet werden. Diese Funktion nimmt ein Datumsobjekt, eine Zeitzone und ein optionaler Zeitraum an.

```Clojure
(with-time-zone "GMT" my-date) ;; Konvertiere my-date in die Zeitzone GMT
(with-time-zone "GMT+2" my-date :hours) ;; Konvertiere my-date in GMT+2 und addiere 2 Stunden
```

Es ist auch möglich, die `with-local-tz` Funktion zu verwenden, um das Datum automatisch in die lokale Zeitzone zu konvertieren.

```Clojure
(def my-date (java.util.Date.))
(with-local-tz my-date) ;; Konvertiere my-date in die lokale Zeitzone
```

Es ist wichtig zu beachten, dass das Format-Argument auch die Zeitzonenausgabe beeinflussen kann. Zum Beispiel:

```Clojure
(format my-date "dd.MM.yyyy HH:mm:ss Z") ;; Ausgabe: "02.08.2019 13:45:21 +0200"
```

Die `Z` Formatierung gibt die Zeitzonendifferenz in Stunden an. Es ist auch möglich, einen benutzerdefinierten Zeitraum in der `format` Funktion anzugeben.

## Siehe auch

- [Clojure Dokumentation zur `format` Funktion](https://clojuredocs.org/clojure.core/format)
- [Offizielle Java-Dokumentation zu `java.util.Date`](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html)
- [Clojure Dokumentation zur `with-time-zone` Funktion](https://clojuredocs.org/clojure.java-time/with-time-zone)
- [Clojure Dokumentation zur `with-local-tz` Funktion](https://clojuredocs.org/clojure.java-time/with-local-tz)