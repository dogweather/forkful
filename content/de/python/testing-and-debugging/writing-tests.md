---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:27.935182-07:00
description: "Das Schreiben von Tests in Python beinhaltet das Erstellen automatisierter\
  \ Skripte, um die Korrektheit Ihres Codes zu validieren. Programmierer tun dies,\u2026"
lastmod: '2024-03-13T22:44:53.382460-06:00'
model: gpt-4-0125-preview
summary: "Das Schreiben von Tests in Python beinhaltet das Erstellen automatisierter\
  \ Skripte, um die Korrektheit Ihres Codes zu validieren. Programmierer tun dies,\u2026"
title: Tests Schreiben
weight: 36
---

## Was & Warum?
Das Schreiben von Tests in Python beinhaltet das Erstellen automatisierter Skripte, um die Korrektheit Ihres Codes zu validieren. Programmierer tun dies, um sicherzustellen, dass ihre Funktionen oder Klassen unter verschiedenen Bedingungen wie erwartet funktionieren, was hilft, Fehler frühzeitig zu erkennen und die Wartung sowie das Refactoring zu erleichtern.

## Wie geht das:
Python kommt mit einem integrierten Modul zum Schreiben von Tests namens `unittest`. So können Sie es verwenden, um eine einfache Funktion zu testen:

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "Sollte 12 sein")

if __name__ == '__main__':
    unittest.main()
```

Wenn Sie dieses Testsript ausführen, sollten Sie eine Ausgabe sehen, die darauf hinweist, dass Ihre Tests bestanden haben (oder fehlgeschlagen sind).

Für modernere und aussagekräftigere Tests können Sie eine Drittanbieter-Bibliothek wie `pytest` verwenden. Zuerst müssen Sie es mit pip installieren:

```shell
pip install pytest
```

Dann können Sie Ihre Tests auf eine einfachere Weise schreiben, ohne etwas ableiten zu müssen:

```python
# Speichern Sie dies in einer Datei namens test_with_pytest.py
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "Sollte 12 sein"
```

Um Ihre Tests mit `pytest` auszuführen, führen Sie einfach aus:

```shell
pytest test_with_pytest.py
```

Sie sollten eine Ausgabe von pytest sehen, die Ihre Testergebnisse anzeigt.
