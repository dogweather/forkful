---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:45.815007-07:00
description: "Att skriva tester i Python inneb\xE4r att skapa automatiserade skript\
  \ f\xF6r att validera korrektheten i din kod. Programmerare g\xF6r detta f\xF6r\
  \ att s\xE4kerst\xE4lla\u2026"
lastmod: '2024-03-13T22:44:37.486631-06:00'
model: gpt-4-0125-preview
summary: "Att skriva tester i Python inneb\xE4r att skapa automatiserade skript f\xF6\
  r att validera korrektheten i din kod."
title: Skriva tester
weight: 36
---

## Hur man gör:
Python kommer med en inbyggd modul för att skriva tester som kallas `unittest`. Så här kan du använda den för att testa en enkel funktion:

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "Borde vara 12")

if __name__ == '__main__':
    unittest.main()
```

När du kör detta testskript bör du se utdata som indikerar att dina tester passerade (eller misslyckades).

För mer moderna och uttrycksfulla tester kan du använda ett tredjepartsbibliotek som `pytest`. Först måste du installera det med pip:

```shell
pip install pytest
```

Sedan kan du skriva dina tester på ett enklare sätt utan att behöva ärva från någon klass:

```python
# Spara detta i en fil med namnet test_with_pytest.py
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "Borde vara 12"
```

För att köra dina tester med `pytest`, exekvera helt enkelt:

```shell
pytest test_with_pytest.py
```

Du bör se utdata från pytest som visar dina testresultat.
