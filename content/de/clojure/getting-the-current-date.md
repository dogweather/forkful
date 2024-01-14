---
title:                "Clojure: Die aktuelle Datumsangabe erhalten"
simple_title:         "Die aktuelle Datumsangabe erhalten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

In der heutigen Zeit ist es wichtig, dass wir wissen, an welchem Datum wir uns befinden. Ob es darum geht, einen Termin zu planen oder die Fälligkeit einer Rechnung zu überprüfen, das aktuelle Datum ist eine wesentliche Information. In diesem Blog-Beitrag werden wir uns ansehen, wie wir mit Clojure das aktuelle Datum abrufen können.

## Wie Funktioniert Es

Um das aktuelle Datum in Clojure abzurufen, können wir die Funktion `now` aus der Bibliothek `clojure.java-time` verwenden. Diese Funktion gibt ein `java.time.LocalDate`-Objekt zurück, welches das Datum in Form von Tag, Monat und Jahr enthält.

```Clojure
(ns blog-post.core
  (:require [java-time :refer [now]]))

(println (now))
```

Dieser Code gibt uns das aktuelle Datum in der folgenden Form zurück: `#<LocalDate 2021-10-20>`

Mithilfe der Funktionen `get-day-of-month`, `get-month` und `get-year` können wir das Datum aufschlüsseln und konkret ausgeben:

```Clojure
(ns blog-post.core
  (:require [java-time :refer [now get-day-of-month get-month get-year]]))

(def datum (now))
(println (str "Heute ist der " (get-day-of-month datum) "." (get-month datum) "." (get-year datum)))
```

Das Ergebnis wäre dann: `Heute ist der 20.10.2021`.

## Tiefere Einblicke

Für diejenigen, die etwas tiefer in das Thema einsteigen möchten, hier ein paar weitere Infos: `clojure.java-time` baut auf der Java-Bibliothek `java.time` auf, welche im Jahr 2014 in Java 8 eingeführt wurde. Diese bietet eine umfangreiche Unterstützung für das Arbeiten mit Datum und Uhrzeit. Mehr Informationen zu `java.time` findet ihr [hier] (https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html).

## Siehe Auch

- [Offizielle Clojure-Website] (https://clojure.org/)
- [Dokumentation zu clojure.java-time] (https://cljdoc.org/d/java-time/java-time/1.0.0/api/clojure.java-time)
- [Clojure-Community-Forum auf Reddit] (https://www.reddit.com/r/Clojure/)