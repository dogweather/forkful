---
title:    "Clojure: Das Abrufen des aktuellen Datums"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum

Das Abrufen des aktuellen Datums ist in der Programmierung eine nützliche Aufgabe und kann in verschiedenen Anwendungsfällen erforderlich sein. Zum Beispiel können wir das Datum in einer Anwendung anzeigen, um die Aktualität von Daten zu überprüfen oder in einer monatlichen Berichtsfunktion verwenden.

# Wie es gemacht wird

Um das aktuelle Datum in Clojure zu bekommen, verwenden wir die Funktion ` (java.util.Date.)` . Diese Funktion erstellt ein Objekt vom Typ `java.util.Date` , das das aktuelle Datum und die Uhrzeit enthält.

```
Clojure (java.util.Date.)
```

Das obige Beispiel gibt das aktuelle Datum und die Uhrzeit als Objekt zurück. Um das Datum lesbarer zu machen, können wir die ` (format)` Funktion verwenden.

```
Clojure (format (java.text.SimpleDateFormat. "dd.MM.yyyy") (java.util.Date.))
```

Dies gibt das Datum in dem Format "TT.MM.JJJJ" aus (zum Beispiel 10.09.2021).

# Tiefentauchen

Es gibt viele Möglichkeiten, das Datum in Clojure weiter zu manipulieren oder es in verschiedene Formate zu konvertieren. Eine Möglichkeit ist die Verwendung der ` (clj-time)` Bibliothek, die eine einfachere und flexiblere Art und Weise bietet, mit Datum und Zeit in Clojure zu arbeiten. Zum Beispiel können wir das gleiche Beispiel wie oben mit dieser Bibliothek wie folgt schreiben:

```
Clojure (require 'clj-time.core)
Clj (d/in (clj-time.core/today) (clj-time.core/formatter "dd.MM.yyyy"))
```

Dies gibt das gleiche Ergebnis wie das vorherige Beispiel zurück. Mit dieser Bibliothek können wir auch das Datum nach unseren Wünschen manipulieren, z.B. indem wir eine bestimmte Anzahl von Tagen oder Monaten hinzufügen oder abziehen.

# Siehe auch

- Dokumentation zur Funktion ` (java.util.Date.)`: https://clojuredocs.org/clojure.java/javadoc/clojure.java.api/. Date
- Dokumentation zur ` (clj-time)` Bibliothek: https://github.com/clj-time/clj-time