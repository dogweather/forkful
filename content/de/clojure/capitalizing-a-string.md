---
title:                "Einen String großschreiben"
html_title:           "Clojure: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Eine Zeichenkette zu kapitalisieren bedeutet, jeden ersten Buchstaben eines Wortes in Großbuchstaben umzuwandeln. Programmierer tun dies oft, um Text lesbar zu machen oder bestimmte Wörter hervorzuheben.

## Wie geht's:

Hier ist eine Beispiel von einer Funktion, die eine Zeichenkette in Clojure kapitalisiert.

```Clojure
(defn capitalize-str [str]
  (->> (clojure.string/split str #" ")
       (map clojure.string/capitalize)
       (clojure.string/join " ")))
```

Verwendung der Funktion:

``` Clojure
(capitalize-str "clojure ist eine Pracht!")
;; Ausgabe: "Clojure Ist Eine Pracht!"
```

## Vertiefung

(1) Historischer Kontext: Das Kapitalisieren von Zeichenketten ist eine typische Funktion in vielen Sprachen, nicht nur in Clojure. Es hat seine Wurzeln in der typografischen Tradition, wichtige Wörter oder Phrasen hervorzuheben.

(2) Alternativen: Abhängig von den Anforderungen der Aufgabe, kann es vorzuziehen sein, nur den ersten Buchstaben eines Satzes zu kapitalisieren, oder jede Zeichenkette völlig in Großbuchstaben umzuwandeln. Für Letztere, `clojure.string/upper-case` wäre eine passende Funktion.

(3) Implementierungsdetails: `clojure.string/capitalize` capitalizes the first character of the string and downcase all the trailing characters. To just capitalize the first character while keeping the rest of the string intact, you'd need a different implementation. The provided `capitalize-str` function splits the string into words, capitalizes each word and then joins the words back together with spaces in between.

## Siehe auch

- [Clojure Cheatsheet, String Functions](https://clojure.org/api/cheatsheet)
- [ClojureDocs on clojure.string](https://clojuredocs.org/clojure.string)
- [Stackoverflow Discussion on Capitalizing Strings](https://stackoverflow.com/questions/9551104/string-capitalization-in-clojure)