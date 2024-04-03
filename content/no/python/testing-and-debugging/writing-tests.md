---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:41.847028-07:00
description: "Hvordan: Python kommer med en innebygd modul for \xE5 skrive tester\
  \ kalt `unittest`. Slik kan du bruke den til \xE5 teste en enkel funksjon."
lastmod: '2024-03-13T22:44:40.364876-06:00'
model: gpt-4-0125-preview
summary: "Python kommer med en innebygd modul for \xE5 skrive tester kalt `unittest`."
title: Skrive tester
weight: 36
---

## Hvordan:
Python kommer med en innebygd modul for å skrive tester kalt `unittest`. Slik kan du bruke den til å teste en enkel funksjon:

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "Skulle vært 12")

if __name__ == '__main__':
    unittest.main()
```

Når du kjører dette testskriptet, bør du se output som indikerer at testene dine bestod (eller feilet).

For mer moderne og uttrykksfulle tester, kan du bruke et tredjepartsbibliotek som `pytest`. Først må du installere det ved hjelp av pip:

```shell
pip install pytest
```

Deretter kan du skrive testene dine på en enklere måte uten å måtte arve fra noe:

```python
# Lagre dette i en fil kalt test_with_pytest.py
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "Skulle vært 12"
```

For å kjøre testene dine med `pytest`, utfører du ganske enkelt:

```shell
pytest test_with_pytest.py
```

Du bør se output fra pytest som viser resultatene dine.
