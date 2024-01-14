---
title:    "Clojure: Umwandeln eines Strings in Kleinbuchstaben"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Warum

Wenn du jemals mit Strings in deinem Clojure Code gearbeitet hast, bist du wahrscheinlich auf die Funktion `clojure.string/lower-case` gestoßen. Aber warum sollte man überhaupt Strings in Kleinbuchstaben umwandeln? Es gibt tatsächlich einige Gründe dafür:

- Formatierung: Oft möchten wir sicherstellen, dass alle Zeichen in einem String in derselben Formatierung sind, sei es in Großbuchstaben oder Kleinbuchstaben.
- Vergleiche: Manchmal müssen wir Strings miteinander vergleichen, und die Vergleichsfunktion ist sensibel bezüglich Groß- und Kleinschreibung. Durch das Umwandeln in Kleinbuchstaben können wir sicherstellen, dass die Vergleiche korrekt durchgeführt werden.
- Datenbereinigung: Beim Parsen von Daten aus verschiedenen Quellen können wir uns nicht auf die Formatierung von Strings verlassen. Das Umwandeln in Kleinbuchstaben ermöglicht es uns, Strings zu normalisieren und eine einheitliche Datenstruktur zu erhalten.

# Wie man Strings in Kleinbuchstaben umwandelt

In Clojure können wir die Funktion `clojure.string/lower-case` verwenden, um einen String in Kleinbuchstaben umzuwandeln. Hier ist ein Beispiel:

```Clojure
(clojure.string/lower-case "HELLO WELT") ; Output: "hello welt"
```

Wir können diese Funktion auch auf Teile eines Strings anwenden, indem wir sie mit `subs` kombinieren. Zum Beispiel:

```Clojure
(clojure.string/lower-case (subs "Hallo Welt" 0 5)) ; Output: "hallo"
```

Eine andere Möglichkeit, Strings in Kleinbuchstaben zu konvertieren, besteht darin, die Methoden `to-lower-case` oder `toLowerCase` auf einem String-Objekt auszuführen. Zum Beispiel:

```Clojure
(.toLowerCase "GUTEN TAG") ; Output: "guten tag"
```

# Tiefere Einblicke

Es ist wichtig zu beachten, dass die Funktion `clojure.string/lower-case` keine rekursive Unterstützung für Unicode-Buchstaben bietet. Das bedeutet, dass Nicht-ASCII-Zeichen möglicherweise nicht korrekt umgewandelt werden. Um dieses Problem zu lösen, können wir die Bibliothek `clojure.string/lower-case` aus der Bibliothek `clojure.string/replace` verwenden. Diese Funktion verwendet reguläre Ausdrücke, um Unicode-Zeichen in Kleinbuchstaben umzuwandeln.

Ein weiteres wichtiges Detail ist, dass die Transformationsfunktionen für Groß- und Kleinschreibung in Clojure für Performance-Optimierungen nicht in der Kontextauswertung ausgewertet werden. Dies bedeutet, dass die Funktion `clojure.string/lower-case` aufgerufen wird, wenn sie innerhalb einer Schleife oder einer Funktion mehrmals aufgerufen wird, was zu einer geringeren Leistung führen kann. In diesem Fall kann es sinnvoll sein, eine lokale Bindung für die Funktion zu erstellen und sie nur einmal auszuwerten.

# Siehe auch

- [Clojure Dokumentation zur Funktion lower-case](https://clojuredocs.org/clojure.string/lower-case)
- [Tutorial zu regulären Ausdrücken in Clojure](https://purelyfunctional.tv/guide/clojure-regular-expressions/)
- [Die offizielle Clojure Webseite](https://clojure.org/)