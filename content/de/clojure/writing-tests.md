---
title:                "Tests schreiben"
html_title:           "Clojure: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Tests sind ein wichtiger Bestandteil des Programmierens, um sicherzustellen, dass der Code ordnungsgemäß funktioniert und mögliche Fehler frühzeitig zu erkennen. Durch das Schreiben von Tests kann man auch die Verständlichkeit und Wartbarkeit des Codes verbessern, was langfristig Zeit und Aufwand sparen kann.

## Wie man Tests schreibt

Das Schreiben von Tests in Clojure ist einfach und intuitiv. Zunächst muss das Modul clojure.test importiert werden:

```Clojure
(require '[clojure.test :refer [deftest is]])
```

Danach kann man das Keyword `deftest` verwenden, um eine Testfunktion zu definieren. Diese Funktion sollte einen beschreibenden Namen haben und die zu testende Funktion als Argument haben. Mit dem Keyword `is` können dann Aussagen über das erwartete Verhalten der Funktion gemacht werden. Hier ein Beispiel:

```Clojure
(deftest test-addition
  (is (= (+ 2 2) 4)))
```

Dieser Test überprüft, ob die Addition von 2 und 2 gleich 4 ergibt. Um den Test auszuführen, kann man die Funktion `run-tests` aufrufen:

```Clojure
(run-tests)
```

Die Ausgabe sollte dann folgendermaßen aussehen:

```Clojure
Testing foo.core
Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

Wenn man möchte, kann man auch aussagekräftigere Fehlermeldungen in den Tests verwenden. Zum Beispiel könnte man die Funktion `is` mit einer Beschreibung versehen:

```Clojure
(deftest test-addition
  (is (=
       (+ 2 2) ; zu testender Code
       4        ; erwartetes Ergebnis
       "2 + 2 sollte 4 ergeben"))) ; Beschreibung
```

Die konkrete Syntax der Tests hängt stark von der zu testenden Funktion und den gewünschten Aussagen ab. Die offizielle Dokumentation von `clojure.test` bietet zahlreiche Beispiele und detailliertere Erklärungen für verschiedene Anwendungsfälle.

## Tiefergehende Informationen

Die Funktionen `deftest` und `is` sind nur die grundlegenden Bausteine für Tests in Clojure. Es gibt noch weitere Funktionen und Konzepte wie z.B. `isnt` für negative Aussagen, `testing` für Gruppierung von Tests und mögliche Mocking-Frameworks. Auch eine Implementierung von Property-Based Testing ist möglich.

Eine wichtige Sache, die man beim Schreiben von Tests beachten sollte, ist die Unabhängigkeit der Tests voneinander. Jeder Test sollte nur eine kleine, aber aussagekräftige Einheit überprüfen, um die Lesbarkeit und Wartbarkeit zu verbessern.

## Siehe auch

- Offizielle Dokumentation von `clojure.test`: https://clojure.github.io/clojure/clojure.test-api.html
- Tutorial über Tests in Clojurescript: https://www.youtube.com/watch?v=uXFUl0zJrjY
- Eigene Erfahrung im Schreiben von Tests: https://www.meineprogrammiererleben.de/2020/05/15/warum-tests-schreiben-in-clojure/