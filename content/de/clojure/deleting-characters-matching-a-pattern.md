---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Clojure-Programmierung: Zeichen Mithilfe eines Musters Löschen

## Was & Warum?
Beim Löschen von Zeichen, die einem Muster entsprechen, geht es darum, bestimmte Teile eines Strings zu entfernen. Programmierer machen das oft, um irrelevante oder störende Daten loszuwerden.

## So geht's:

Clojure bietet dafür die Funktion `clojure.string/replace`. Sie nimmt 3 Argumente: den ursprünglichen String, das Muster und den Ersetzungstext. Um Zeichen zu löschen, geben Sie einfach "" als Ersetzungstext an:

```clojure
(require '[clojure.string :as str])

(let [original "abc123" pattern "\\d" replacement ""]
  (str/replace original pattern replacement))
```

Ausgabe:

```clojure
"abc"
```

Mit diesem Code lösen Sie alle Ziffern aus dem Originalstring.

## Tiefere Instanz:

Obwohl `clojure.string/replace` weit verbreitet und sehr leistungsfähig ist, gibt es Alternativen zum Löschen von Zeichen:

1. `clojure.string/replace-first`: Ähnlich wie `replace`, aber es ersetzt nur das erste Auftreten des Musters.

2. Mit der Java interop: Clojure läuft auf der JVM, daher können Sie Java-Methoden nutzen, wie z.B. `String.replaceAll`.

```clojure
(let [original "abc123" pattern "\\d" replacement ""]
  (.replaceAll original pattern replacement))
```

Es ist auch nützlich zu wissen, dass `clojure.string/replace` das Muster als regulären Ausdruck behandelt. Daher müssen spezielle Zeichen durch Doppelschwenkstriche (`\\`) maskiert werden.

## Siehe Auch:

- Clojure API: `clojure.string/replace`. Siehe [Clojurescript API-Dokumentation](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/replace)
- Wann und Wie Java interop benutzt werden sollte: [Clojure for the Brave and True](https://www.braveclojure.com/java/)