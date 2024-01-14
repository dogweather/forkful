---
title:                "Clojure: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Wenn man in der Softwareentwicklung tätig ist, ist es wichtig, sich mit dem Thema Testen auseinanderzusetzen. Durch das Schreiben von Tests kann man sicherstellen, dass der Code funktioniert und spart langfristig Zeit und Nerven bei der Fehlersuche. In diesem Blog-Beitrag erfährst du, wie du effektive Tests in Clojure schreiben kannst.

## Wie geht's

Zuerst müssen wir die Test-Bibliothek `clojure.test` importieren. Dann können wir unsere Funktionen testen, indem wir sie innerhalb von `def`-Blöcken definieren und mit `is` Aussagen über ihre erwarteten Ausgaben abgeben. Schauen wir uns dazu ein Beispiel an:

```Clojure
(ns testing.blog
  (:require [clojure.test :refer [deftest is]]))

; Funktion, die das Quadrat einer Zahl berechnet
(defn square [x]
  (* x x))

; Test für die square-Funktion
(deftest square-test
  (is (= (square 3) 9)) ; erwartete Ausgabe: 9
  (is (= (square -5) 25)) ; erwartete Ausgabe: 25
  (is (= (square 4) 16))) ; erwartete Ausgabe: 16
```

Wenn du diese Code-Beispiele ausführst, solltest du sehen, dass alle Tests erfolgreich sind! Es ist eine gute Praxis, jede Funktion in deinem Code mit mindestens einem Test abzudecken, um sicherzustellen, dass sie immer die erwarteten Ergebnisse liefert.

## Tiefergehende Informationen

Es gibt viele Möglichkeiten, Tests in Clojure zu schreiben, einschließlich der Verwendung von Mocking-Frameworks und Property Testing. Um tiefer in dieses Thema einzusteigen, empfehle ich dir folgende Ressourcen:

- Clojure's offizielle Dokumentation zu `clojure.test`: [(en) https://clojure.github.io/clojure/clojure.test-api.html](https://clojure.github.io/clojure/clojure.test-api.html) / [(de) https://clojure.github.io/clojure/clojure.test-api-de.html](https://clojure.github.io/clojure/clojure.test-api-de.html)
- Rich Hickey's Präsentation über "Spec-ulation": [(en) https://www.infoq.com/presentations/clojure-spec/ ](https://www.infoq.com/presentations/clojure-spec/) / [(de) https://www.youtube.com/watch?v=oh6L5STFAj0 ](https://www.youtube.com/watch?v=oh6L5STFAj0)

## Siehe auch

- [(en) https://gist.github.com/alexanderkyte/93884185a1f7d50383147a6baa89cc01](https://gist.github.com/alexanderkyte/93884185a1f7d50383147a6baa89cc01)
- [(en) https://purelyfunctional.tv/guide/es6-conversion-testing-with-clojure/](https://purelyfunctional.tv/guide/es6-conversion-testing-with-clojure/) / [(de) https://purelyfunctional.tv/wissensbasis/es6-konvertierung-testen-mit-clojure/](https://purelyfunctional.tv/wissensbasis/es6-konvertierung-testen-mit-clojure/)