---
title:                "Pisanie testów"
date:                  2024-01-19
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-tests.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie testów to proces weryfikacji, czy nasz kod robi to, co mamy na celu. Programiści piszą testy, żeby unikać błędów, upraszczać wprowadzanie zmian i zwiększać jakość kodu.

## Jak to zrobić:
```Python
import unittest

class SimpleTest(unittest.TestCase):
    def test_addition(self):
        result = 1 + 1
        self.assertEqual(result, 2)

if __name__ == '__main__':
    unittest.main()
```
Sample Output:
```
.
----------------------------------------------------------------------
Ran 1 test in 0.000s

OK
```

## Dogłębna analiza
Pisanie testów zaczęto rozważać z konieczności zachowania jakości w czasie ewolucji oprogramowania. Alternatywami są TDD (Test-Driven Development) czy BDD (Behavior-Driven Development), które zmieniają perspektywę pisania testów. Python używa `unittest`, `pytest`, `nose`, ale `pytest` jest najpopularniejszy ze względu na prostotę i wszechstronność.

## Zobacz również
- Dokumentacja `unittest`: https://docs.python.org/3/library/unittest.html
- `pytest`: https://docs.pytest.org/en/latest/
- Artykuł "Test-Driven Development" na Wiki: https://en.wikipedia.org/wiki/Test-driven_development
