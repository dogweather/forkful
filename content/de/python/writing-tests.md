---
title:                "Python: Tests schreiben"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Wenn man Python Programme schreibt, gibt es oft die Versuchung, direkt zum eigentlichen Programmcode überzugehen und die Tests auszulassen. Doch Testen ist ein wichtiger Teil des Entwicklungsprozesses und kann Zeit und Nerven sparen. In diesem Blogbeitrag erfahren Sie warum es sich lohnt, Tests in ihre Python Programme einzubauen.

## So geht's

Um Tests in Python zu schreiben, gibt es verschiedene Tools wie zum Beispiel die Module `unittest` oder `pytest`. Hier ist ein einfaches Beispiel mit `unittest`:

```python
import unittest

def square(x):
    return x ** 2

class SquareTestCase(unittest.TestCase):

    def test_square(self):
        self.assertEqual(square(5), 25)

    def test_negative(self):
        self.assertEqual(square(-5), 25)
```

Ausgeführt mit `python -m unittest -v` sollte dieses Skript zwei erfolgreiche Tests ausgeben:

```
test_negative (__main__.SquareTestCase) ... ok
test_square (__main__.SquareTestCase) ... ok

----------------------------------------------------------------------
Ran 2 tests in 0.000s

OK
```

## Tieferer Einblick

Tests sind nicht nur hilfreich, um zu überprüfen, ob der Code richtig funktioniert. Sie können auch dazu beitragen, mögliche Fehlerquellen zu identifizieren und das Verständnis des Programms zu verbessern. Durch die gezielte Verwendung von `assert`-Statements können Sie genau festlegen, welche Bedingungen erfüllt sein müssen, um die Tests zu bestehen. Auch das Testen von komplexen Datenstrukturen wie Listen oder Dictionaries ist möglich.

Um Tests in Ihre Entwicklung zu integrieren, ist es hilfreich, einen Test-Driven-Development Ansatz zu verfolgen. Das bedeutet, dass man zuerst die Tests schreibt und danach den Code, der diese Tests bestehen lässt. Dadurch haben Sie immer eine kontinuierliche Rückmeldung über den Zustand Ihres Codes und können Fehler schnell erkennen und beheben.

## Siehe auch

- [Python Documentation: unittest](https://docs.python.org/3/library/unittest.html)
- [Python Testing with pytest](https://realpython.com/python-testing/)
- [Test Driven Development (TDD): Beispiel in Python](https://www.tutorialspoint.com/test-driven-development-example-in-python)