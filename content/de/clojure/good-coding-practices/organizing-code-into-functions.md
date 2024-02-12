---
title:                "Code in Funktionen organisieren"
aliases:
- /de/clojure/organizing-code-into-functions/
date:                  2024-01-26T01:09:44.816218-07:00
model:                 gpt-4-1106-preview
simple_title:         "Code in Funktionen organisieren"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Was & Warum?

Code in Funktionen zu organisieren bedeutet, Blöcke von Code zu verpacken, die spezifische Aufgaben erfüllen. Dadurch wird dein Code sauber, leichter zu warten und ein Kinderspiel für andere Entwickler zu lesen.

## Wie man es macht:

Funktionen in Clojure werden mit `defn` definiert, gefolgt von einem Namen, Parametern und einem Körper. Hier ein schnelles Beispiel.

```Clojure
(defn greet [name]
  (str "Hallo, " name "!"))

(greet "Alex") ; => "Hallo, Alex!"
```

Nehmen wir nun an, wir möchten die Fläche eines Rechtecks berechnen. Statt alles zusammenzuwurschteln, trennen wir es in zwei Funktionen:

```Clojure
(defn area [length width]
  (* length width))

(defn print-area [length width]
  (println "Die Fläche beträgt:" (area length width)))

(print-area 3 4) ; => Die Fläche beträgt: 12
```

## Tiefere Einblicke

Früher hämmerten Programmierer einfach all ihre Logik in einen einzigen Block. Es war unschön. Dann kam die strukturierte Programmierung und Funktionen wurden zum Ding. In Clojure ist jede Funktion erstklassig – du kannst sie herumschleudern wie jeden anderen Wert.

Alternativen? Manche Leute könnten mit Multimethoden oder höherwertigen Funktionen herumspielen, aber das sind nur Gewürze im Funktionseintopf.

Alles in den Details einer Funktion: sie sind in Clojure unveränderlich, was Nebeneffekte weniger wahrscheinlich macht. Sie stützen sich stark auf Rekursion anstelle von typischen Schleifen, was gut mit den funktionalen Paradigmen der Sprache harmoniert.

## Siehe auch

- Der eigene Leitfaden von Clojure: https://clojure.org/guides/learn/functions
- Grundlagen der Funktionalen Programmierung: https://www.braveclojure.com/core-functions-in-depth/
- Rich Hickeys Vorträge: https://changelog.com/posts/rich-hickeys-greatest-hits - für Einblicke in die Philosophie von Clojure.
