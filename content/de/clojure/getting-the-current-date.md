---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:13:50.171026-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Programmieren ist das Abrufen des aktuellen Datums ein häufig erforderlicher Vorgang, um zeitabhängige Funktionen zu implementieren. Man braucht es für alles Mögliche, von Zeitstempeln in Log-Dateien bis hin zur Überprüfung der Gültigkeit von Verträgen.

## So geht's:

Um das aktuelle Datum in Clojure zu bekommen, nutzt man das Java-Interoperabilitätsfeature, weil Clojure auf der Java Virtual Machine (JVM) läuft. Hier ein einfaches Beispiel:

```clojure
(import java.util.Date)
(import java.text.SimpleDateFormat)

(defn get-current-date []
  (let [format (SimpleDateFormat. "dd.MM.yyyy")]
    (.format format (Date.))))

(println (get-current-date)) ; Beispiel Ausgabe: 01.04.2023
```

Oder, für nur das Datum ohne Zeit:

```clojure
(import java.time.LocalDate)

(defn get-current-date-only []
  (.toString (LocalDate/now)))

(println (get-current-date-only)) ; Beispiel Ausgabe: 2023-04-01
```

## Tiefergehend:

Historisch gesehen basiert die Datumsbehandlung in Clojure, wie in vielen anderen JVM-Sprachen, auf den Java-Klassen `java.util.Date` und `java.util.Calendar`. Seit Java 8 gibt es das neue `java.time`-Paket mit `LocalDate`, `LocalTime` und `LocalDateTime` für eine verbesserte und intuitivere Arbeit mit Daten und Zeiten.

Alternativen zur Standard-Java-Bibliothek in Clojure umfassen Bibliotheken wie `clj-time`, eine Wrapper-Bibliothek, die jedoch zunehmend obsolet wird, da `java.time` eine verbesserte Funktionalität bietet. Für einfache Anwendungen reicht die Java-Interoperabilität aus, für komplexere Datumsmanipulationen kann `java.time` direkt oder über eine Clojure-Bibliothek genutzt werden, die diese Funktionalitäten einbindet.

Beim Implementieren sollte man Zeitzonen bedenken. Während `Date` und `LocalDate.now` das System-Standarddatum und -zeit zurückgeben, kann man mit `ZonedDateTime` oder `OffsetDateTime` explizite Zeitangaben machen.

## Siehe auch:

- Die `java.time`-Dokumentation für tiefergehende Informationen: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Die Clojure-Dokumentation zur Java-Interoperabilität: https://clojure.org/reference/java_interop
- Einblick in die Clojure-Bibliothek `clj-time`: https://github.com/clj-time/clj-time
