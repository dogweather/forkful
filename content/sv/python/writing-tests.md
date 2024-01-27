---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Att skriva tester innebär att koda små program som kör din kod för att kontrollera att allt funkar. Programmerare gör detta för att säkerställa att koden är robust, förhindrar fel och underlättar framtida underhåll.

## How to:
Python använder ofta `unittest` för att skriva tester. Så här ser ett grundläggande testexempel ut:

```python
import unittest

def add(a, b):
    return a + b

class TestAddition(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(3, 4), 7)

if __name__ == '__main__':
    unittest.main()
```
Kör detta script i terminalen så ska du se följande output:

```
.
----------------------------------------------------------------------
Ran 1 test in 0.000s

OK
```

## Deep Dive
Att skriva tester är en del av utvecklingsprocessen som har funnits sedan programmeringens barndom. Alternativ till `unittest` i Python inkluderar `pytest` och `nose`, som kan erbjuda mer avancerade funktioner och enklare syntax. Viktiga detaljer att notera när man skriver tester inkluderar isolering av tester (så de inte påverkar varandra), täckning (så mycket kod som möjligt testas) och mockning (att efterapa delar av systemet).

## See Also
För att lära dig mer om testning i Python, besök:
- Python's officiella dokumentation om `unittest`: https://docs.python.org/3/library/unittest.html
- `pytest` dokumentationen: https://docs.pytest.org/en/latest/
- En guide till mockning i tester: https://docs.python.org/3/library/unittest.mock.html
- Om koddäckning och verktyget `coverage.py`: https://coverage.readthedocs.io/
