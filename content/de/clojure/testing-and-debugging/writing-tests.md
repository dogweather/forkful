---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:58.077866-07:00
description: "Tests in Clojure zu schreiben, \xE4hnlich wie in anderen Programmiersprachen,\
  \ umfasst das Erstellen von dediziertem Code, um zu verifizieren, dass Ihre\u2026"
lastmod: '2024-03-13T22:44:53.423032-06:00'
model: gpt-4-0125-preview
summary: "Tests in Clojure zu schreiben, \xE4hnlich wie in anderen Programmiersprachen,\
  \ umfasst das Erstellen von dediziertem Code, um zu verifizieren, dass Ihre Hauptcodebasis\
  \ wie erwartet funktioniert."
title: Tests Schreiben
weight: 36
---

## Wie:
Clojure nutzt die JVM und unterstützt verschiedene Test-Frameworks. Ein häufig verwendetes eingebautes Bibliothek ist jedoch `clojure.test`. Hier ist ein einfaches Beispiel:

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "Additionsfunktionalität"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
Nach dem Ausführen dieses Tests würden Sie eine Ausgabe ähnlich wie folgt sehen:

```
Testing example.test

Ran 2 tests containing 2 assertions.
0 failures, 0 errors.
```

Für diejenigen, die reichhaltigere Funktionen suchen, kann man Drittanbieter-Bibliotheken wie `Midje` oder `test.check` nutzen. So könnten Sie Midje für einen ähnlichen Test verwenden:

Fügen Sie zunächst Midje zu Ihren project.clj Abhängigkeiten hinzu:
```clojure
[midje "1.9.9"]
```

Dann könnte Ihr Test mit Midje so aussehen:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "Teste Addition"
  (add 2 2) => 4
  (add 3 4) => 7)
```

Beim Ausführen des Tests über Midje mit `lein midje` würde die Ausgabe etwa folgendes anzeigen:

```
All checks (2) succeeded.
```
