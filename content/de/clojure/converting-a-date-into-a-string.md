---
title:    "Clojure: Umwandlung eines Datums in einen String"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum
In der Programmierung gibt es häufig die Notwendigkeit, ein Datum in einen String umzuwandeln. Dies kann zum Beispiel nötig sein, um Benutzereingaben in einem bestimmten Format zu akzeptieren oder um Daten in einer Datenbank abzuspeichern. In diesem Blogbeitrag werden wir uns anschauen, wie man dies in Clojure erreichen kann.

## How To
Um ein Datum in einen String umzuwandeln, können wir die `time/format` Funktion verwenden. Diese Funktion akzeptiert zwei Argumente: das Datum selbst und ein Formatierungsmuster. Dabei ist das Formatierungsmuster optional und standardmäßig auf `"yyyy-MM-dd"` gesetzt. Schauen wir uns dazu ein Beispiel an:

```Clojure
(require '[clojure.java-time :as time])

(time/format (java.time.LocalDate/now)) ; Ausgabe: "2021-12-14"
```

In diesem Beispiel haben wir die aktuelle Datum mit `java.time.LocalDate/now` erhalten und dieses dann mit der `time/format` Funktion in einen String umgewandelt. Wie bereits erwähnt, wurde standardmäßig das Formatierungsmuster `"yyyy-MM-dd"` verwendet.

Natürlich können wir das Formatierungsmuster nach unseren Bedürfnissen anpassen. Hier ein Beispiel, bei dem wir das Datum im Format "dd.MM.yyyy" ausgeben möchten:

```Clojure
(time/format (java.time.LocalDate/now) "dd.MM.yyyy") ; Ausgabe: "14.12.2021"
```

Es ist auch möglich, andere Zeitangaben, wie zum Beispiel eine bestimmte Uhrzeit, in den String einzubinden. Dazu können wir `java.time.LocalDateTime` verwenden:

```Clojure
(time/format (java.time.LocalDateTime/now) "dd.MM.yyyy HH:mm") ; Ausgabe: "14.12.2021 12:00"
```

## Deep Dive
Nun wollen wir uns etwas genauer anschauen, wie das Formatierungsmuster funktioniert. Das Muster besteht aus bestimmten Zeichen, die für verschiedene Zeitangaben stehen. Hier eine Übersicht der wichtigsten Zeichen:

- `yyyy` - vierstellige Jahreszahl
- `MM` - zweistelliger Monat
- `dd` - zweistelliger Tag
- `HH` - zweistellige Stunde (im 24-Stunden-Format)
- `mm` - zweistellige Minute
- `ss` - zweistellige Sekunde
- `S` - Millisekunden
- `z` - Zeitzone

Es ist auch möglich, diese Zeichen zu kombinieren, um ein gewünschtes Format zu erhalten. Zum Beispiel können wir eine Uhrzeit mit Millisekunden und Zeitzone ausgeben:

```Clojure
(time/format (java.time.LocalDateTime/now) "HH:mm:ss.S z") ; Ausgabe: "12:00:00.000 GMT+01:00"
```

Eine ausführliche Liste aller verfügbaren Zeichen findet sich in der [Clojure-Dokumentation](https://clojuredocs.org/clojure.java-time/format).

## Siehe auch
- [Offizielle Clojure Dokumentation](https://clojuredocs.org/clojure.java-time/format)
- [Java-Dokumentation zu java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)