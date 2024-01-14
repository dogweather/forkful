---
title:    "Clojure: Tests schreiben"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich die Mühe machen, Tests für sein Clojure-Programm zu schreiben? Nun, es gibt viele gute Gründe dafür. Erstens können Tests dabei helfen, Bugs frühzeitig zu erkennen und zu beheben, bevor sie in der Produktion auftreten. Auch können Tests dazu beitragen, die Code-Qualität zu verbessern und die Wartbarkeit des Programms zu erhöhen. Nicht zuletzt bieten sie auch eine Art der Dokumentation, indem sie zeigen, wie das Programm richtig verwendet werden sollte.

## How To

Jetzt wollen wir uns anschauen, wie man in Clojure Tests schreibt. Das folgende Beispiel zeigt einen einfachen Test, der prüft, ob die Funktion `add` zwei Zahlen korrekt addiert.

```Clojure
(deftest test-addition
  (testing "Addition von zwei positiven Zahlen"
    (is (= 4 (add 2 2)))))
```

In diesem Beispiel verwenden wir die Test-Bibliothek `clojure.test`, welche bereits in Clojure integriert ist. Zuerst definieren wir einen Test mit `deftest` und geben ihm einen sprechenden Namen. Innerhalb des Tests können wir dann mit der Funktion `testing` eine bestimmte Eigenschaft oder Verhaltensweise unserer Funktion `add` prüfen. Mit `is` testen wir, ob das erwartete Ergebnis von `add` mit dem tatsächlichen Ergebnis übereinstimmt. In diesem Fall erwarten wir, dass `2 + 2` gleich `4` ergibt. Führen wir den Test aus, sehen wir hoffentlich ein grünes Häkchen, welches bestätigt, dass der Test erfolgreich war.

## Deep Dive

Nun wollen wir etwas tiefer in die Materie eintauchen. Beim Schreiben von Tests gibt es einige bewährte Praktiken, die es zu beachten gilt. Zum Beispiel sollte jeder Test unabhängig und isoliert sein, d.h. er sollte nicht von anderen Tests oder von äußeren Faktoren beeinflusst werden. Auch sollte jeder Test klar und verständlich benannt sein, damit man sofort erkennt, welches Verhalten getestet wird.

Eine weitere hilfreiche Technik beim Schreiben von Tests ist das Mocken, d.h. das Ersetzen von bestimmten Teilen des Codes, die sich nicht direkt testen lassen. Dadurch können wir unser Programm in einer kontrollierten Umgebung testen und mögliche Abhängigkeiten oder Fehlerquellen eliminieren. Es gibt verschiedene Test-Frameworks, die Mocking unterstützen, zum Beispiel `testdouble` oder `cljmock`.

## Siehe auch

- [Clojure Test-Dokumentation](https://clojure.org/guides/test)
- [Praktiken zum Schreiben von Tests in Clojure](https://vvvvalvalval.github.io/posts/2018-04-25-test-intemporal.html)
- [Testdouble Framework](https://github.com/profile/testdouble)