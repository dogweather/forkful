---
title:                "Skriva tester"
aliases:
- sv/python/writing-tests.md
date:                  2024-02-03T19:31:45.815007-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester i Python innebär att skapa automatiserade skript för att validera korrektheten i din kod. Programmerare gör detta för att säkerställa att deras funktioner eller klasser fungerar som förväntat under olika förhållanden, vilket hjälper till att fånga upp fel tidigt och underlättar enklare underhåll och refaktorisering.

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
