---
title:                "Tests schreiben"
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?

Tests schreiben bedeutet, Code-Ausschnitte zu erstellen, die überprüfen, ob einzelne Funktionen oder Module deiner Anwendung korrekt arbeiten. Programmierer*innen machen das, um Fehler schnell zu finden, die Qualität des Codes zu sichern und das Vertrauen in die Software zu stärken.

## Anleitung:

In Clojure nutzt man häufig die `clojure.test`-Bibliothek für Tests. Hier ist ein einfaches Beispiel:

```clojure
(ns mein-projekt.core-test
  (:require [clojure.test :refer :all]
            [mein-projekt.core :refer :all]))

(deftest addition-test
  (testing "Addition funktioniert"
    (is (= 4 (addiere 2 2)))))

; Test ausführen
(run-tests)
```

Wenn du dieses Skript in deiner REPL ausführst, solltest du so etwas wie das folgende sehen:

```
lein test mein-projekt.core-test

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

## Tiefergehende Infos:

Tests in Clojure haben ihre Wurzeln in der LISP-Tradition, die Interaktivität und schnelles Feedback betont. Alternativen zu `clojure.test` sind etwa Midje oder Expectations, die andere Herangehensweisen und Syntax bieten. Beim Schreiben von Tests geht es oft um die Balance zwischen Unit-Tests, die einzelne Komponenten prüfen, und Integrationstests, die das Zusammenspiel zwischen Komponenten testen.

## Siehe auch:

- [Clojure Testing Framework](https://clojure.github.io/clojure/clojure.test-api.html)
- [Midje on GitHub](https://github.com/marick/Midje)
- [Expectations on GitHub](https://github.com/clojure-expectations/expectations)