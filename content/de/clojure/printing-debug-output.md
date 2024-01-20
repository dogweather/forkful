---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# Programmierung in Clojure: Debug-Ausgaben in Kürze

## Was & Warum?

Beim Drucken von Debug-Ausgaben handelt es sich im Grunde genommen darum, Informationen während der Ausführung eines Programms auf der Konsole oder in Log-Dateien auszugeben. Es hilft uns, das Verhalten des Programms zu überwachen und Fehler in unserem Code zu identifizieren und zu beheben.

## Wie geht's:

In Clojure verwenden wir meistens die Funktion `println` um Debug-Ausgaben zu drucken.

```Clojure
(defn hello [name]
  (println "Debug: bevor wir grüßen")
  (println (str "Hallo, " name))
  (println "Debug: nachdem wir gegrüßt haben"))

(hello "Clojure")
```

Die Ausgabe wird sein:
```
Debug: bevor wir grüßen
Hallo, Clojure
Debug: nachdem wir gegrüßt haben
```

## Tiefer Eintauchen

Historisch gesehen, war das Drucken von Debug-Ausgaben eine der ersten Methoden zur Fehlersuche in Programmen. Its simplicity and straightforwardness have made it a long-standing tool in every programmer's arsenal.

Es gibt jedoch Alternativen zum Drucken von Debug-Ausgaben in Clojure, wie zum Beispiel das Hinzufügen von Logging-Funktionen durch Bibliotheken wie `clojure.tools.logging`.

Ein tiefere Überlegung ist, dass die Verwendung von `println` in mehreren Threads zu Konflikten führen kann aufgrund von Buffers. Es ist also oft besser, auf spezielle Logging-Libraries auszuweichen, die für solche Situationen vorbereitet sind.

## Siehe Auch

- Official Clojure Documentation: https://clojure.org/guides/getting_started
- Clojure's println function: https://clojuredocs.org/clojure.core/println
- Clojure.tools.logging library: https://github.com/clojure/tools.logging

Viel Spaß beim Programmieren!