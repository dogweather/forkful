---
title:                "Clojure: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Warum

In der heutigen digitalen Welt ist es oft notwendig, große Mengen von Text zu bearbeiten und zu ändern. Das kann von der einfachen Korrektur von Rechtschreibfehlern bis hin zur komplexen Manipulation von Datenbanken reichen. Zum Glück bietet Clojure eine einfache und elegante Möglichkeit, Textsuche und -ersetzung durchzuführen.

# Wie man python-zu-clojure nutzt

Die Suche und Ersetzung in Clojure erfolgt über die Funktion `clojure.string/replace`, die eine Zeichenkette, ein Muster und einen Ersatz als Argumente akzeptiert. Hier ist ein Beispiel, um alle Vorkommen von "Hund" durch "Katze" in einem Satz zu ersetzen:

```Clojure
(clojure.string/replace "Ich mag Hunde." #"Hund" "Katze")
```

Die Ausgabe wäre: "Ich mag Katzen."

Man kann auch reguläre Ausdrücke verwenden, um komplexere Suchmuster anzugeben. Zum Beispiel kann man alle Zahlen in einer Zeichenkette durch Sternchen ersetzen:

```Clojure
(clojure.string/replace "Die Antwort auf alle Fragen ist 42." #"\d+" "*")
```

Die Ausgabe wäre: "Die Antwort auf alle Fragen ist *."

# Tiefes Tauchen

Clojure bietet auch die Funktion `clojure.string/replace-first`, die nur das erste Vorkommen des Musters ersetzt. Außerdem gibt es `clojure.string/replace-last`, um nur das letzte Vorkommen zu ersetzen, sowie `clojure.string/replace-nth`, um ein bestimmtes Vorkommen basierend auf seiner Position in der Zeichenkette zu ersetzen.

Ein weiteres nützliches Feature ist die Möglichkeit, eine Funktion als Ersatz zu verwenden. Diese Funktion erhält das gefundene Muster als Argument und kann dann eine Ersatzzeichenkette zurückgeben. So kann man beispielsweise alle Wörter in einer Zeichenkette in Großbuchstaben konvertieren:

```Clojure
(clojure.string/replace "Ich liebe es, Clojure zu programmieren." #"\w+" clojure.string/upper-case)
```

Die Ausgabe wäre: "ICH LIEBE ES, CLOJURE ZU PROGRAMMIEREN."

# Siehe auch

* [Clojure String Dokumentation](https://clojuredocs.org/clojure.string/replace)
* [Reguläre Ausdrücke in Clojure](https://clojure.org/guides/regular_expressions)