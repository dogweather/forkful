---
title:                "Umgang mit komplexen Zahlen"
date:                  2024-01-26T04:38:32.135728-07:00
model:                 gpt-4-0125-preview
simple_title:         "Umgang mit komplexen Zahlen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen erweitern die reellen Zahlen um einen zusätzlichen Teil, die imaginäre Einheit 'i'. Programmierer verwenden sie in verschiedenen Bereichen, einschließlich Signalverarbeitung, Elektromagnetentheorie und Fraktalen, wo Berechnungen, die die Quadratwurzel einer negativen Zahl beinhalten, Routine sind.

## Wie:
Clojure bietet eingebaute Unterstützung für komplexe Zahlen durch die Utility-Klasse `clojure.lang.Numbers`. Verwenden Sie `complex`, um komplexe Zahlen zu erstellen und arithmetische Operationen durchzuführen.

```clojure
;; Erstellen von komplexen Zahlen
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; Addition
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; Subtraktion
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; Multiplikation
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; Division
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; Konjugation
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## Tiefere Einblicke
Komplexe Zahlen wurden im 18. Jahrhundert von Mathematikern wie Gauss und Euler formalisiert. Obwohl sie anfänglich auf Skepsis stießen, sind sie seitdem in der modernen Wissenschaft und Technik unverzichtbar geworden. Clojure verfügt nicht über einen nativen komplexen Zahlen-Typ wie einige Sprachen (z.B. Python), aber das enthaltene Java-Interop kann die notwendigen Operationen über die Klasse `clojure.lang.Numbers` handhaben.

Javas `java.lang.Complex` ist eine robuste Alternative, die mehr Funktionen und potenzielle Optimierungen bietet. Die Host-Interoperabilität von Clojure erleichtert die Arbeit mit Java-Bibliotheken.

Unter der Haube umfasst die Arithmetik komplexer Zahlen das Addieren und Multiplizieren der realen und imaginären Teile, wobei die Schlüsselregel lautet `i^2 = -1`. Die Division komplexer Zahlen kann komplizierter sein, da in der Regel das Konjugat benötigt wird, um die Division durch komplexe Zahlen zu vermeiden.

## Siehe auch
- Die ClojureDocs, für eine schnelle Referenz: https://clojuredocs.org/
- Die Java API für `java.lang.Complex`: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- Die Wikipedia-Seite zu komplexen Zahlen für mathematisch Interessierte: https://en.wikipedia.org/wiki/Complex_number