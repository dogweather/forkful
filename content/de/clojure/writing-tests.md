---
title:                "Clojure: Das Schreiben von Tests"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Tests ist ein wichtiger Bestandteil beim Entwickeln von Clojure-Programmen. Tests ermöglichen es Entwicklern, ihre Codebasis zu überprüfen und sicherzustellen, dass alle Funktionen wie erwartet funktionieren. Dadurch wird die Qualität des Codes erhöht und Bugs können frühzeitig erkannt und behoben werden. Dies spart Zeit und Mühe in der späteren Entwicklungsphase.

## Wie geht es?

Zum Schreiben von Tests in Clojure gibt es verschiedene Möglichkeiten. Eine gängige Methode ist die Verwendung des integrierten Testframeworks von Clojure, clojure.test. Hier ist ein Beispiel, wie eine einfache Testfunktion in Clojure aussehen könnte:

```Clojure
(ns test-funktionen
  (:require [clojure.test :refer :all]))

(defn addieren [a b]
  (+ a b))

(deftest test-addieren
  (is (= 4 (addieren 2 2)))
  (is (= 10 (addieren 6 4))))
```

In diesem Beispiel wurde eine Funktion namens "addieren" definiert, die zwei Zahlen addiert. Dann wurde eine Testfunktion ("test-addieren") erstellt, die überprüft, ob die richtigen Ergebnisse zurückgegeben werden.

Um diese Tests auszuführen, müssen wir in der Konsole in das Verzeichnis des Projekts wechseln und den Befehl "lein test" ausführen. Dies wird uns sagen, ob unsere Tests erfolgreich waren oder nicht. Wenn ein Test fehlschlägt, wird eine Fehlermeldung mit allen relevanten Informationen angezeigt, um das Problem zu beheben.

## Tiefer gehen

Obwohl das obige Beispiel eine einfache Einführung in das Testen in Clojure bietet, gibt es noch viel mehr zu entdecken. Zum Beispiel gibt es verschiedene assert-Funktionen, die in clojure.test verwendet werden können, um verschiedene Arten von Tests durchzuführen. Darüber hinaus gibt es auch andere Testframeworks, die spezifische Vorteile und Funktionen bieten.

Es ist auch wichtig zu beachten, dass das Testen nicht nur beim Schreiben von Funktionen stattfinden sollte, sondern auch beim Überprüfen von Schnittstellen und Integrationen mit anderen Teilen des Codes. Wenn mehrere Entwickler an einem Projekt arbeiten, sollten auch Continuous Integration-Tools verwendet werden, um sicherzustellen, dass Änderungen keine bestehenden Funktionen beeinträchtigen.

## Siehe auch

- Offizielle Clojure-Testdokumentation: https://clojure.github.io/clojure/clojure.test-api.html
- Ein Tutorium zum Schreiben von Tests in Clojure: https://www.braveclojure.com/testing/
- Vorteile des Testens in Clojure: https://www.youtube.com/watch?v=P56JVpERsXc