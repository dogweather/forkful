---
title:                "Python: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-tests.md"
---

{{< edit_this_page >}}

# Warum Tests schreiben?

Tests sind ein wichtiger Bestandteil der Softwareentwicklung und ermöglichen es uns, unsere Programme auf mögliche Fehler zu überprüfen. Sie helfen uns, Sicherheit und Qualität in unseren Code zu bringen und erleichtern die Wartung und Erweiterung unserer Anwendungen. Ohne Tests können wir nie sicher sein, dass unser Code zuverlässig funktioniert. Daher ist es wichtig, Tests zu schreiben, um eine stabile und fehlerfreie Software zu erstellen.

## Wie schreibt man Tests?

Das Schreiben von Tests in Python ist einfach und kann in wenigen Schritten durchgeführt werden. Zunächst müssen wir das Python-Modul `unittest` importieren, das uns die nötigen Werkzeuge zur Verfügung stellt, um Tests zu schreiben. Dann können wir unsere Tests in einer speziellen Testklasse schreiben, die von `unittest.TestCase` erbt. Innerhalb dieser Klasse können wir verschiedene Testmethoden definieren, die jeweils einen Teil unseres Codes überprüfen. Zum Beispiel könnte eine Testmethode überprüfen, ob eine bestimmte Funktion den erwarteten Wert zurückgibt.

Hier ist ein einfaches Beispiel, wie wir mit `unittest` eine Funktion `add()` testen könnten:

```Python
import unittest

def add(x, y):
    return x + y

class TestAddFunction(unittest.TestCase):
    def test_positive_numbers(self):
        result = add(2, 3)
        self.assertEqual(result, 5)
        
    def test_negative_numbers(self):
        result = add(-3, -5)
        self.assertEqual(result, -8)
```

In diesem Beispiel haben wir eine Testklasse `TestAddFunction` erstellt, die zwei Testmethoden enthält. Die `test_positive_numbers()` Methode überprüft, ob die Funktion `add()` den korrekten Wert für positive Zahlen zurückgibt. Die `test_negative_numbers()` Methode überprüft das Verhalten für negative Zahlen. Wir verwenden die Methode `assertEqual()` um zu überprüfen, ob der erwartete Wert mit dem tatsächlichen Ergebnis übereinstimmt.

Um unsere Tests auszuführen, können wir das `unittest`-Modul direkt ausführen oder eine Test Runner-Anwendung wie `pytest` verwenden. Wir erhalten dann eine Übersicht über alle durchgeführten Tests und ob sie erfolgreich waren.

## Tiefergehende Informationen über das Schreiben von Tests

Es gibt verschiedene Arten von Tests, die wir in unserem Code schreiben können. Einige Beispiele sind Unittests, Integrationstests, Funktionstests oder Akzeptanztests. Jede dieser Testarten hat ihre eigene Bedeutung und wird in verschiedenen Entwicklungsphasen eingesetzt.

Unittests sind in der Regel Tests auf niedrigstem Niveau, die einzelne Funktionen oder Methoden überprüfen. Integrationstests testen, wie verschiedene Komponenten zusammenarbeiten und kommunizieren. Funktionstests überprüfen die korrekte Ausführung einer bestimmten Funktion oder Aufgabe. Und Akzeptanztests überprüfen, ob die Anwendung den Anforderungen der Benutzer entspricht.

Beim Schreiben von Tests ist es wichtig, gut lesbaren und verständlichen Code zu schreiben. Achte darauf, aussagekräftige Namen für deine Tests zu verwenden und kommentiere ggf. komplexe Testabläufe, um anderen Entwicklern zu helfen, den Zweck des Tests zu verstehen.

Ein weiterer wichtiger Aspekt beim Testen ist die Testabdeckung. Dies beschreibt, wie viel unseres Codes durch Tests abgedeckt wird. Eine hohe Testabdeckung bedeutet, dass fast alle Teile unseres Codes mindestens einmal getestet wurden. Eine hohe Testabdeckung hilft uns, potenzielle Fehlerquellen ausfindig zu machen und unseren Code qualitativ hochwertiger zu machen.

## Siehe auch

- [Python unittest Module](https://docs.python.org/3/library/unittest.html)
- [pytest Test Runner](https://docs.pytest.org/en/stable/)