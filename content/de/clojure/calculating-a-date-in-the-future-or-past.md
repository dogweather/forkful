---
title:    "Clojure: Eine beliebige Zukunft oder Vergangenheitsdatum berechnen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Warum
Jeder muss manchmal ein Datum in der Zukunft oder Vergangenheit berechnen, sei es für eine Bewerbungsfrist oder ein wichtiges Ereignis. Mit Clojure können wir diesen Prozess einfacher und effizienter gestalten.

# Wie gehts
Wir verwenden die Funktion `java.util.Calendar` zusammen mit den Funktionen `add` und `get` um ein Datum in der Zukunft oder Vergangenheit zu berechnen.

```
;; Berechnung eines zukünftigen Datums: 14 Tage in Zukunft
(def today (java.util.Calendar/getInstance))
(.add today java.util.Calendar/DATE 14)
(print (str "Das Datum in 14 Tagen ist: " (.get today java.util.Calendar/DAY_OF_MONTH) "." (.get today java.util.Calendar/MONTH) "." (.get today java.util.Calendar/YEAR)))
```

```
;; Berechnung eines vergangenen Datums: vor 3 Jahren
(def today (java.util.Calendar/getInstance))
(.add today java.util.Calendar/YEAR -3)
(print (str "Das Datum vor 3 Jahren war: " (.get today java.util.Calendar/DAY_OF_MONTH) "." (.get today java.util.Calendar/MONTH) "." (.get today java.util.Calendar/YEAR)))
```

## Deep Dive
Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, müssen wir zuerst eine Instanz von `java.util.Calendar` erstellen. Dann können wir die Funktion `add` verwenden, um eine bestimmte Zeiteinheit (z.B. Tage, Monate oder Jahre) zum aktuellen Datum hinzuzufügen oder davon abziehen. Abschließend können wir die Funktion `get` verwenden, um das gewünschte Datum aus dem Kalenderobjekt zu extrahieren.

# Siehe Auch
- [Offizielle Clojure Dokumentation für Date-Berechnungen](https://clojure.org/api/java.util.Calendar)
- [Youtube-Tutorial: Clojure für Anfänger](https://www.youtube.com/watch?v=TurbraO_Js0)