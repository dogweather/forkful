---
title:                "Tests schreiben"
date:                  2024-01-19
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"

category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Tests schreiben bedeutet, Code zu erstellen, der deinen Hauptcode automatisch ausführt und überprüft, ob alles wie erwartet funktioniert. Programmierer machen das, um Fehler zu vermeiden, sicherzustellen, dass alles reibungslos läuft, und um die Qualität ihrer Software zu gewährleisten. 

## How to:
Ein einfacher Python-Test mit `unittest`, einer integrierten Testbibliothek:

```python
import unittest

def summe(a, b):
    return a + b

class TestSumme(unittest.TestCase):
    def test_funktion_summe(self):
        self.assertEqual(summe(3, 4), 7)

if __name__ == '__main__':
    unittest.main()
```

Ausgabe beim Ausführen der Tests:

```
.
----------------------------------------------------------------------
Ran 1 test in 0.001s

OK
```

## Deep Dive
Unit-Tests, wie im Beispiel, sind schon seit den Anfängen der Softwareentwicklung im Einsatz. Alternativen zu `unittest` sind zum Beispiel `pytest` und `nose`, die flexiblere Ansätze und Syntax bieten können. Wichtig ist, Tests isoliert und reproduzierbar zu gestalten, damit sie verlässlich die Funktionsfähigkeit des Codes sichern.

## See Also
- Die offizielle `unittest` Dokumentation: https://docs.python.org/3/library/unittest.html
- `pytest`, eine mächtige, Drittanbieter-Testbibliothek: https://pytest.org/
- Artikel über Test-Driven Development: https://realpython.com/tdd-start-to-finish/
