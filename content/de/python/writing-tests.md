---
title:    "Python: Test schreiben"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren geht es nicht nur darum, funktionierenden Code zu schreiben, sondern auch darum, sicherzustellen, dass der Code zuverlässig ist und erwartungsgemäß funktioniert. Das Schreiben von Tests ist eine Möglichkeit, dieses Ziel zu erreichen.

## Wie geht man vor?

Um Tests in Python zu schreiben, verwenden wir das Modul "unittest". Wir definieren eine Klasse, die von "unittest.TestCase" erbt, und fügen dann verschiedene Testfunktionen hinzu, die jeweils einen bestimmten Teil des Codes testen. Hier ist ein Beispiel für eine einfache Testsuite:

```Python
import unittest

class TestCalculator(unittest.TestCase):
    def test_addition(self):
        self.assertEqual(5+10, 15) # Testet, ob die Addition von 5 und 10 das erwartete Ergebnis 15 liefert

    def test_subtraction(self):
        self.assertEqual(20-5, 15) # Testet, ob die Subtraktion von 20 und 5 das erwartete Ergebnis 15 liefert

if __name__ == '__main__':
    unittest.main()
```

Sie können die Tests ausführen, indem Sie das Skript ausführen. Wenn alle Tests erfolgreich sind, sehen Sie keine Ausgabe. Wenn jedoch ein Test fehlschlägt, wird eine Fehlermeldung angezeigt. Hier ist ein Beispiel:

```
======================================================================
FAIL: test_subtraction (__main__.TestCalculator)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "my_test_file.py", line 9, in test_subtraction
    self.assertEqual(20-5, 16)
AssertionError: 15 != 16

----------------------------------------------------------------------
Ran 2 tests in 0.000s

FAILED (failures=1)
```

## Tief tauchen

Tests ermöglichen es uns, unseren Code auf mehrere Arten zu testen und sicherzustellen, dass er in verschiedenen Szenarien funktioniert. Wir können z.B. "Edge Cases" testen, also unerwartete Eingaben, die unser Code möglicherweise nicht verarbeiten kann. Wir können auch "Mocking" verwenden, um bestimmte Teile des Codes zu simulieren und zu testen.

Es gibt auch verschiedene Arten von Tests, wie z.B. Unit-Tests, Integrationstests und Funktionstests. Jede Art von Test hat ihre eigene Funktion und sollte in bestimmten Situationen verwendet werden.

## Siehe auch

- [Offizielle Python-Dokumentation zu "unittest"](https://docs.python.org/3/library/unittest.html)
- [Ein Tutorial zu Tests in Python](https://realpython.com/python-testing/#types-of-python-tests)
- [Ein Artikel über die Wichtigkeit von Tests in der Softwareentwicklung](https://www.toptal.com/qa/why-test-infrastructure-matters-in-developing-software)