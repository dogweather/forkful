---
title:                "Unterscheiden von Unterzeichenketten"
html_title:           "Clojure: Unterscheiden von Unterzeichenketten"
simple_title:         "Unterscheiden von Unterzeichenketten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Du fragst dich vielleicht, warum du überhaupt daran interessiert sein solltest, Substrings in Clojure zu extracten. Nun, das kann verschiedene Gründe haben. Vielleicht möchtest du einen bestimmten Teil einer Zeichenkette isolieren, um sie weiter zu verarbeiten oder zu analysieren. Oder vielleicht möchtest du einfach nur deine Programmierkenntnisse erweitern und neue Techniken kennenlernen. Egal aus welchem Grund, das Extracten von Substrings kann in vielen Situationen nützlich sein.

## So geht's

Um Substrings in Clojure zu extracten, benötigst du die `subs` Funktion. Diese erwartet drei Argumente: die ursprüngliche Zeichenkette, den Startindex und den Endindex. Schauen wir uns ein Beispiel an:

```Clojure
(def s "Hallo Welt!")
(subs s 0 5)
```

Dieses Code-Snippet gibt `"Hallo"` aus, da es die ersten fünf Zeichen der Zeichenkette `s` auswählt. Beachte, dass der erste Index als `0` angegeben wird und der Endindex nicht inklusive ist. Du kannst auch negative Indizes verwenden, um die Zeichen von hinten auszuwählen. Zum Beispiel gibt `(subs s -6 -1)` `"Welt"` aus.

Du kannst auch die `str` Funktion verwenden, um Substrings direkt zu verketten. Hier ein Beispiel:

```Clojure
(def first-name "John")
(def last-name "Doe")
(str "My name is " (subs first-name 0 1) "." (subs last-name 0 1) ".")
```

Dieser Code ergibt die Zeichenkette `"My name is J.D."`. Du kannst auch die `count` Funktion verwenden, um die Länge einer Zeichenkette zu bestimmen und sie für die Extraktion von Substrings zu nutzen. Probiere es selbst aus!

## Tiefer tauchen

Die `subs` Funktion ist sehr nützlich, aber es gibt noch einige weitere Funktionen, die beim Extracten von Substrings in Clojure hilfreich sein können. Die `slice` Funktion zum Beispiel bietet ein anderes Format für die Angabe der Indizes und ist besonders gut geeignet, wenn du wiederkehrende Muster in deinem Code hast. Die `substring` Funktion hingegen ermöglicht es dir, den Endindex inklusive zu halten und erleichtert damit oft die Erstellung von Substrings.

Darüber hinaus gibt es auch Funktionen, die gezielt auf bestimmte Datentypen abzielen. Die `subvec` Funktion zum Beispiel extrahiert Substrings aus Vektoren und die `re-find` Funktion ist besonders nützlich für die Extraktion von Substrings, die einem bestimmten Muster folgen.

Insgesamt gibt es viele verschiedene Möglichkeiten, Substrings in Clojure zu extracten. Experimentiere mit verschiedenen Funktionen und finde heraus, welche am besten zu deinem Code und deinen Bedürfnissen passen.

## Siehe auch

- Offizielle Clojure Dokumentation für `subs`: https://clojuredocs.org/clojure.core/subs
- Clojure Cheatsheet für String-Operationen: https://clojure.org/api/cheatsheet#StringOperations