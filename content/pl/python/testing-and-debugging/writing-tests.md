---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:38.510973-07:00
description: "Jak to zrobi\u0107: Python posiada wbudowany modu\u0142 do pisania test\xF3\
  w, nazwany `unittest`. Oto, jak mo\u017Cesz go u\u017Cy\u0107 do przetestowania\
  \ prostej funkcji."
lastmod: '2024-03-13T22:44:34.953084-06:00'
model: gpt-4-0125-preview
summary: "Python posiada wbudowany modu\u0142 do pisania test\xF3w, nazwany `unittest`."
title: "Pisanie test\xF3w"
weight: 36
---

## Jak to zrobić:
Python posiada wbudowany moduł do pisania testów, nazwany `unittest`. Oto, jak możesz go użyć do przetestowania prostej funkcji:

```python
import unittest

def add(a, b):
    return a + b

class TestAddFunction(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(1, 2), 3)
        self.assertEqual(add(-1, 1), 0)
        self.assertNotEqual(add(10, 2), 12, "Powinno być 12")

if __name__ == '__main__':
    unittest.main()
```

Kiedy uruchomisz ten skrypt testowy, powinieneś zobaczyć wyniki wskazujące, czy twoje testy przeszły (lub nie).

Dla nowocześniejszych i bardziej wyrazistych testów możesz użyć zewnętrznej biblioteki, takiej jak `pytest`. Najpierw musisz ją zainstalować za pomocą pip:

```shell
pip install pytest
```

Następnie możesz napisać swoje testy w prostszy sposób, bez konieczności dziedziczenia czegokolwiek:

```python
# Zapisz to w pliku o nazwie test_with_pytest.py
def add(a, b):
    return a + b

def test_add():
    assert add(1, 2) == 3
    assert add(-1, 1) == 0
    assert add(10, 2) != 12, "Powinno być 12"
```

Aby uruchomić swoje testy z `pytest`, wystarczy wykonać:

```shell
pytest test_with_pytest.py
```

Powinieneś zobaczyć wyniki testów pokazane przez pytest.
