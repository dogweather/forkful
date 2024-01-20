---
title:                "Testen schreiben"
html_title:           "Python: Testen schreiben"
simple_title:         "Testen schreiben"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-tests.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Schreiben von Tests ist eine der wichtigsten Aufgaben eines Programmierers. Es ist ein Prozess, bei dem man überprüft, ob der Code richtig funktioniert und die erwarteten Ergebnisse liefert. Es ist wichtig, Tests zu schreiben, um sicherzustellen, dass der Code zuverlässig ist und möglichen Fehlern vorzubeugen.

# Wie geht das?
Die beste Möglichkeit, um zu verstehen, wie man Tests schreibt, ist anhand von Beispielen. Im Folgenden sind zwei grundlegende Beispiele für das Schreiben von Tests in Python aufgeführt.

```Python
# Importieren der unittest-Bibliothek
import unittest

# Erstellen eines einfachen Tests
class SimpleTest(unittest.TestCase):

    # Definieren einer Testfunktion
    def test_sum(self):
        # Berechnen der Summe von 2 und 3 erwartete Ergebnis: 5 
        result = 2 + 3
        # Überprüfen, ob das Ergebnis 5 ist
        self.assertEqual(result, 5)

# Ausführen der Tests
if __name__ == '__main__':
    unittest.main()
```

```Python
# Importieren der pytest-Bibliothek
import pytest

# Erstellen eines einfachen Tests mit pytest
def test_division():
    # Berechnen der Division von 10 durch 2 erwartete Ergebnis: 5
    result = 10 / 2
    # Überprüfen, ob das Ergebnis 5 ist
    assert result == 5
```

# Tiefere Einblicke:
## Historischer Kontext:
Der Einsatz von Tests hat sich in den letzten Jahren in der Softwareentwicklung immer weiter verbreitet. Früher wurden Tests manuell durchgeführt, was zu vielen Fehlern führen konnte. Mit dem Aufkommen von automatisierten Tests ist es nun möglich, schneller und zuverlässiger zu testen.

## Alternativen:
Es gibt verschiedene Arten von Tests, die in der Softwareentwicklung verwendet werden. Neben einfachen Einheitstests, wie in den oben gezeigten Beispielen, gibt es auch Integrationstests, Systemtests und Akzeptanztests. Je nach Anforderungen und Komplexität des Projekts können verschiedene Arten von Tests sinnvoll sein.

## Implementierungsdetails:
Das Schreiben von Tests erfordert ein gutes Verständnis der zu testenden Funktionen und des erwarteten Verhaltens. Es ist wichtig, gut lesbaren und wartbaren Code zu schreiben, um die Tests effektiv gestalten zu können. Zudem sollte man die Tests regelmäßig ausführen, um mögliche Fehler frühzeitig zu erkennen und zu beheben.

# Siehe auch:
- [Das pytest-Framework](https://docs.pytest.org/en/latest/)
- [Video-Tutorial: Test-driven Development in Python](https://www.youtube.com/watch?v=IN6j2QMkegA)