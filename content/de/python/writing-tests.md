---
title:    "Python: Tests schreiben"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Warum

Der Prozess des Testens ist ein wichtiger Teil des Programmierens. Es hilft dabei, Fehler in unserem Code zu erkennen und zu beheben, bevor wir ihn in Produktion bringen. Durch ein gutes Testing-Verständnis können wir die Qualität unseres Codes verbessern und damit auch die Benutzererfahrung. In diesem Artikel werden wir uns darauf konzentrieren, wie wir Tests in Python schreiben können, um sicherzustellen, dass unser Code fehlerfrei und zuverlässig ist.

## Wie man Tests in Python schreibt

Das Schreiben von Tests in Python ist eine relativ einfache Aufgabe. Wir verwenden dazu normalerweise das `unittest`-Modul, das Teil der Standardbibliothek von Python ist. Zunächst müssen wir unsere Tests in einer separaten Datei mit dem Namen `test_<name>.py` speichern. Dann importieren wir das `unittest`-Modul und definieren eine Klasse mit dem Namen `Test<name>` und erben von der Klasse `unittest.TestCase`. Innerhalb dieser Klasse können wir dann verschiedene Methoden definieren, die jeweils einen spezifischen Aspekt unseres Codes testen. Sehen wir uns an einem Beispiel an:

```Python
import unittest

# Unsere Klasse zum Testen der divide() Funktion
class TestDivision(unittest.TestCase):

    # Test, ob die divide() Funktion zwei Zahlen korrekt teilt
    def test_divide(self):
        self.assertEqual(divide(8, 2), 4)

    # Test, ob die divide() Funktion eine Fehlermeldung ausgibt, wenn der zweite Parameter 0 ist
    def test_divide_by_zero(self):
        with self.assertRaises(ZeroDivisionError):
            divide(10, 0)
    
    # Weitere Tests können hier hinzugefügt werden

# Eine Funktion, die zwei Zahlen teilt
def divide(x, y):
    return x / y

# Führt alle unsere Tests aus
if __name__ == '__main__':
    unittest.main()
```

Wie im obigen Beispiel gezeigt, verwenden wir die `assertEqual()`-Methode, um zu überprüfen, ob die Ausgabe unserer `divide()`-Funktion mit dem erwarteten Wert übereinstimmt. Wir können auch die `assertRaises()`-Methode verwenden, um zu überprüfen, ob unsere Funktion eine erwartete Fehlermeldung ausgibt. Am Ende rufen wir `unittest.main()` auf, um alle unsere Tests auszuführen.

## Vertiefung

Das waren nur einige Grundlagen, wie man Tests in Python schreibt. Es gibt noch viele weitere Konzepte und Funktionen, die in diesem Bereich verwendet werden können. Zum Beispiel können wir das `mock`-Modul verwenden, um Testdaten zu simulieren, oder das `coverage`-Modul, um die Testabdeckung zu analysieren. Es ist auch wichtig zu beachten, dass Tests nicht nur für einzelne Funktionen geschrieben werden können, sondern auch für ganze Programme oder Module.

Das Schreiben von Tests ist eine kontinuierliche und iterative Aufgabe. Es ist wichtig, sie regelmäßig auszuführen und auch neue Tests hinzuzufügen, um sicherzustellen, dass unser Code weiterhin fehlerfrei bleibt. Durch das Testen unseres Codes können wir auch die Lesbarkeit und Erweiterbarkeit verbessern, da wir uns darauf konzentrieren, einzelne Teile unseres Codes unabhängig voneinander zu testen und zu verbessern.

## Siehe auch

- [Die offizielle Dokumentation zu unittest](https://docs.python.org/de/3/library/unittest.html)
- [Ein umfassendes Tutorial zu Python-Tests von Real Python](https://realpython.com/python-testing/)
- [Ein Blog-Beitrag über das Testen von Python-Code mit Mocks](https://medium.com/python-pandemonium/python-mocking-you-are-a-tricksy-beast-6c4c9ededd78)