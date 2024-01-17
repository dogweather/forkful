---
title:                "Tests schreiben."
html_title:           "Clojure: Tests schreiben."
simple_title:         "Tests schreiben."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Schreiben von Tests ist ein wichtiger Bestandteil des Programmierens. Es ist ein Prozess, bei dem Programmierer ihre Code-Funktionen auf korrekte Funktionalität überprüfen. Durch den Testen ihrer Code-Implementierung können Programmierer sicherstellen, dass ihr Code richtig funktioniert und potenzielle Fehler aufdecken.

## Wie:
Um Tests in Clojure zu schreiben, müssen Sie die Standardbibliothek "clojure.test" importieren. Dann können Sie "deftest" verwenden, um eine Test-Funktion zu erstellen. In dieser Funktion können Sie Ihre Code-Funktion aufrufen und prüfen, ob das Ergebnis den erwarteten Ausgabewert liefert, indem Sie "is" verwenden. Zum Beispiel:

```Clojure
(require '[clojure.test :refer :all])

(deftest add-test
  (is (= 5 (+ 2 3))))
```

Sie können auch "testing" verwenden, um mehrere Tests innerhalb einer Funktion zu schreiben und "testing-vars" verwenden, um mehrere Funktionen zu testen. Um Ihre Tests auszuführen, können Sie "run-tests" aufrufen. Das Ergebnis wird in der Konsole ausgegeben.

## Tiefentauchen:
Testen hat in der Softwareentwicklung eine lange Geschichte. Früher waren manuelle Tests die einzige Möglichkeit, um Code zu überprüfen, aber mit zunehmendem Komplexitätsgrad der Code wurde die Notwendigkeit von Automatisierung immer wichtiger. Neben der Standardbibliothek "clojure.test" gibt es auch alternative Frameworks wie "expectations" und "speclj" für das Schreiben von Tests in Clojure.

## Siehe auch:
- Offizielle Clojure-Test-Dokumentation: https://clojure.github.io/clojure/clojure.test-api.html
- "expectations" Test-Framework: https://github.com/jaycfields/expectations
- "speclj" Test-Framework: https://github.com/slagyr/speclj