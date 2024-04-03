---
date: 2024-01-20 17:42:00.967776-07:00
description: "Das L\xF6schen von Zeichen, die einem Muster entsprechen, bezeichnet\
  \ man, wenn man bestimmte Zeichen aus Strings entfernt, die auf ein gegebenes Muster\u2026"
lastmod: '2024-03-13T22:44:53.404778-06:00'
model: gpt-4-1106-preview
summary: "Das L\xF6schen von Zeichen, die einem Muster entsprechen, bezeichnet man,\
  \ wenn man bestimmte Zeichen aus Strings entfernt, die auf ein gegebenes Muster\
  \ passen."
title: "L\xF6schen von Zeichen, die einem Muster entsprechen"
weight: 5
---

## How to:
```Clojure
(defn delete-matching-chars [pattern string]
  (clojure.string/replace string (re-pattern pattern) ""))

;; Beispiel: Entferne alle Ziffern aus dem String
(println (delete-matching-chars "\\d+" "Heute ist der 4. April 2023"))
;; Ausgabe: Heute ist der . April 

;; Beispiel: Entferne alle nicht-alphanumerischen Zeichen
(println (delete-matching-chars "[^\\w]" "Clojure 1.10! Fantastisch, oder?"))
;; Ausgabe: Clojure 110 Fantastisch oder
```

## Deep Dive
Das Löschen von Zeichen nach einem Muster hat in der Programmierung eine lange Geschichte. Ursprünglich in den 1950ern in Zusammenhang mit dem Text-Editor ed komputiert, ist es jetzt ein Grundbaustein in den meisten modernen Programmiersprachen. In Clojure, einer modernen Lisp-Dialekt, nutzt man `clojure.string/replace` zusammen mit regulären Ausdrücken, um diese Funktionalität umzusetzen. Die Java-Virtual-Maschine (JVM), auf der Clojure läuft, bietet robuste Unterstützung für reguläre Ausdrücke, was Clojure-Entwickler stark von profitieren können. Alternativ könnte man auch `filter`-Funktionen verwenden, aber für einfache Zeichenersetzungen sind reguläre Ausdrücke oft effizienter und lesbarer.

## See Also
- [ClojureDocs: clojure.string/replace](https://clojuredocs.org/clojure.string/replace)
- [Clojure from the ground up: regular expressions](https://aphyr.com/posts/305-clojure-from-the-ground-up-regular-expressions)
- [Java Platform SE 8 - Pattern](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
