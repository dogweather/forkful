---
title:                "Vergleich von zwei Datumsangaben"
html_title:           "Clojure: Vergleich von zwei Datumsangaben"
simple_title:         "Vergleich von zwei Datumsangaben"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum
Warum sollte man sich mit dem Vergleichen von zwei Daten beschäftigen? Ganz einfach: Wenn man in einem Programm zwei verschiedene Zeitpunkte oder Zeitintervalle vergleichen möchte, z.B. um festzustellen, welches Datum früher oder später ist.

# How To
Um zwei Daten in Clojure zu vergleichen, können wir die Funktion `compare` verwenden. Diese nimmt zwei Argumente entgegen und gibt einen Integer zurück, der angibt, ob das erste Argument kleiner, größer oder gleich dem zweiten Argument ist. Hier ein Beispiel:

```Clojure
(compare 01.01.2021 01.02.2021) ;; gibt -1 zurück, da das erste Datum kleiner ist
```
Leicht zu merken: -1 bedeutet kleiner, 1 bedeutet größer und 0 steht für gleich. Wenn wir also prüfen wollen, ob ein Datum später als ein anderes ist, können wir einfach die Funktion `>`, `<` oder `=` verwenden:

```Clojure
(> 01.02.2021 01.01.2021) ;; gibt true zurück, da das erste Datum später ist
```

# Deep Dive
In Clojure ist ein Datum im Grunde nur ein spezielles Objekt, das wir mit `java.util.Date` oder `java.time.LocalDate` erstellen können. Es ist wichtig zu beachten, dass diese Objekte immutable sind, d.h. sie können nicht direkt verändert werden. Daher ist es auch nicht möglich, z.B. 2 Tage zu einem Datum dazu zu addieren, da dies ein neues Datum zurückgibt.

Wenn wir jedoch mit `LocalDate` arbeiten, können wir die Funktion `plus` verwenden, um ein neues Datum mit einem bestimmten Zeitintervall zu erstellen. Zum Beispiel:

```Clojure
(plus 01.01.2021 (days 2)) ;; gibt 03.01.2021 zurück
```

# Siehe auch
- [Offizielle Clojure Dokumentation über Datum und Zeit](https://clojuredocs.org/clojure.java-time)
- [Eine umfangreiche Einführung in Clojure auf Deutsch](https://clojure.org/about/translations)