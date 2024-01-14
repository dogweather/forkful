---
title:    "Clojure: Aktuelles Datum erhalten"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Warum?

Das Abrufen des aktuellen Datums kann für verschiedene Anwendungsfälle nützlich sein, wie z.B. das Erstellen von Zeitstempeln in Logs, die Berechnung von Alter oder die Planung von wiederkehrenden Aufgaben.

## Wie es geht

In Clojure gibt es verschiedene Möglichkeiten, das aktuelle Datum zu erhalten. Eine einfache Möglichkeit ist die Verwendung der Funktion `today`, die in der Bibliothek `clj-time` enthalten ist.

```Clojure
(use 'clj-time.core)
(today)
```

Dies gibt das aktuelle Datum als `org.joda.time.LocalDate`-Objekt zurück, das weiter verwendet werden kann, um das Datum in einem bestimmten Format auszugeben.

```Clojure
(local-date-string (today) "dd.MM.yyyy")
;; Output: "12.04.2021"
```

Alternativ kann das aktuelle Datum mit der eingebauten Funktion `now` abgerufen werden, die einen `java.util.Date` zurückgibt.

```Clojure
(now)
```

Die Rückgabe kann dann mit der Funktion `format` in ein bestimmtes Datumsformat umgewandelt werden.

```Clojure
(format (now) "dd.MM.yyyy")
;; Output: "12.04.2021"
```

## Tiefentauchen

Im Hintergrund arbeitet Clojure mit dem Joda-Time-Framework, das eine umfangreiche Unterstützung für Datum und Zeit bietet, einschließlich Zeitzonen und Ära. Das `clj-time`-Paket bietet eine bequeme Abstraktion für die Verwendung in Clojure.

Es ist auch möglich, das aktuelle Datum mit verschiedenen Zeitangaben zu kombinieren, z.B. um das Datum und die Uhrzeit zu erhalten.

```Clojure
(local-date-time (now))
```

## Siehe auch

- Joda-Time Dokumentation: https://www.joda.org/joda-time/
- Clojure-Docs für `clj-time`: https://cljdoc.org/d/clj-time/clj-time/0.15.2/doc/readme
- Offizielles Clojure-Tutorial: https://clojure.org/guides/getting_started