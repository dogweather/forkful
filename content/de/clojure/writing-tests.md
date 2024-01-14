---
title:    "Clojure: Tests schreiben"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Die Verwendung von Tests ist ein wichtiger Bestandteil des Programmierens in jeder Sprache, einschließlich Clojure. Durch das Schreiben von Tests können Sie sicherstellen, dass Ihr Code ordnungsgemäß funktioniert und auf bestimmte Eingaben reagiert. Außerdem können Tests dazu beitragen, Fehler frühzeitig zu erkennen und die Qualität Ihres Codes langfristig zu verbessern.

## Wie man Tests schreibt

Um Tests in Clojure zu schreiben, verwenden Sie die Bibliothek "clojure.test". Um einen Testfall zu erstellen, definieren Sie eine Funktion mit dem Präfix "test-", die mit der Funktion "is" mindestens einen Vergleich durchführt. Zum Beispiel:

```Clojure
(defn test-addition
  (is (= 4 (+ 2 2)))
  (is (= 10 (+ 5 5))))
```

Um den Test auszuführen, verwenden Sie die Funktion "run-tests" und geben Sie den Namen der Testdatei als Argument an. Die Ausgabe wird Ihnen mitteilen, ob die Tests erfolgreich waren oder ob Fehler aufgetreten sind. Ein Beispiel dafür, wie die Ausgabe aussehen könnte, finden Sie hier:

```Clojure
Testing tests.core
Ran 2 tests containing 6 assertions.
0 failures, 0 errors.
```

Sie können auch Funktionen verwenden, um bestimmte Eingaben für Ihre Tests zu generieren, und dann mit der Funktion "are" mehrere Vergleiche gleichzeitig durchführen. Ein Beispiel sehen Sie hier:

```Clojure
(defn add [x y]
  (+ x y))

(defn test-addition
  (are [x y] (is (= x (+ y 0)))
       1 1
       2 2
       3 3))
```

## Tiefenbehandlung

Beim Schreiben von Tests ist es wichtig, eine gute Abdeckung zu erreichen, um sicherzustellen, dass alle Bereiche Ihres Codes getestet werden. Sie sollten auch versuchen, fehlerhafte Bedingungen zu testen, um sicherzustellen, dass Ihr Code auch dann korrekt funktioniert, wenn unerwartete Eingaben auftreten.

Es kann auch hilfreich sein, Ihre Tests in kleinere Einheiten aufzuteilen und jede Funktion separat zu testen. Auf diese Weise können Sie besser nachvollziehen, wo mögliche Fehler auftreten und diese gezielter beheben.

## Siehe auch

- [Offizielle Clojure Dokumentation zu Tests](https://clojure.org/guides/testing)
- [Eine Einführung in Clojure Tests von PurelyFunctional.tv](https://purelyfunctional.tv/guide/how-to-test-your-clojure-code/)