---
title:                "Clojure: Eine Datumsangabe in einen String umwandeln."
simple_title:         "Eine Datumsangabe in einen String umwandeln."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals versucht haben, ein Datum in eine Zeichenkette umzuwandeln, sind Sie vielleicht auf Probleme gestoßen. Oder vielleicht möchten Sie einfach nur wissen, wie es funktioniert. In jedem Fall ist es ein wichtiges Konzept, das jedem Clojure-Programmierer bekannt sein sollte.

## Anleitung

Um ein Datum in eine Zeichenkette umzuwandeln, können Sie die `str` Funktion verwenden. Dies ist eine sehr nützliche Funktion, die eine beliebige Anzahl von Argumenten akzeptiert und diese zu einer Zeichenkette zusammenfügt. Zum Beispiel:

```Clojure
(str 2019 "/" 11 "/" 15) ;; Gibt "2019/11/15" aus
```

Sie können auch die Funktionen `format` oder `date` aus der `java.util.Date`-Klasse verwenden, um ein Datum in einem bestimmten Format zurückzugeben. Hier ist ein Beispiel unter Verwendung von `format`:

```Clojure
(format (java.util.Date.) "dd.MM.yyyy") ;; Gibt das aktuelle Datum in "Tag.Monat.Jahr"-Format aus, z.B. "15.11.2019"
```

## Tiefere Einblicke

Wenn Sie sich eingehender mit der Umwandlung von Datumsobjekten in Zeichenketten beschäftigen möchten, gibt es einige Dinge zu beachten. Zum Beispiel können Sie das `java.text.SimpleDateFormat`-Objekt verwenden, um ein gewünschtes Datumsformat festzulegen. Hier ist ein Beispiel:

```Clojure
(def sdf (java.text.SimpleDateFormat. "dd.MM.yy"))

(.format sdf (java.util.Date.)) ;; Gibt das aktuelle Datum in "Tag.Monat.Jahr" Format mit 2-stelliger Jahreszahl aus, z.B. "15.11.19"
```

Sie können auch die `java.time`-API verwenden, die in Java 8 eingeführt wurde, um mit Datum und Uhrzeit in Clojure zu arbeiten. Diese API bietet mehr Flexibilität und Funktionalität als die alten Klassen `java.util.Date` und `java.text.SimpleDateFormat`.

## Siehe auch

- [Offizielle Clojure-Dokumentation über das Formatieren von Datum und Zeit](https://clojure.org/guides/dates)
- [Blog-Post über die Java 8 Date and Time API in Clojure](https://techbeacon.com/app-dev-testing/whats-new-java-8-date-time)
- [Stack Overflow-Antworten auf Fragen zur Umwandlung von Datum in Zeichenkette in Clojure](https://stackoverflow.com/questions/43119472/convert-clojure-date-time-to-string/43120230)