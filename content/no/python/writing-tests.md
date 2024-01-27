---
title:                "Skriving av tester"
date:                  2024-01-19
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving av tester handler om å lage automatiserte kontroller som sikrer at koden fungerer som forventet. Programmerere gjør dette for å avdekke feil tidlig, forbedre kodekvaliteten og unngå fremtidige problemer.

## Hvordan:
Python har et innebygd modul kalt `unittest` for å skrive og kjøre tester. Her er et eksempel på en enkel test:

```python
import unittest

def add(a, b):
    return a + b

class TestMathFunctions(unittest.TestCase):
    def test_add(self):
        self.assertEqual(add(3, 4), 7)
        self.assertEqual(add(-1, 1), 0)
        self.assertEqual(add(-1, -1), -2)

if __name__ == '__main__':
    unittest.main()
```

Kjør testen, og du får en output som bekrefter om testene er vellykkede eller ikke.

## Dypdykk:
Før `unittest` var det vanligere å skrive tester uten et standard rammeverk, eller å bruke tredjepartsbiblioteker som `nose` eller `pytest`. Disse bibliotekene gir flere funksjoner og enklere syntax, men `unittest` er fortsatt populært fordi det er en del av Python's standardbibliotek. Testene blir skrevet i klasser som arver fra `unittest.TestCase`, og hver testmetode starter med ordet `test`.

## Se Også:
- Pythons offisielle dokumentasjon om testing: https://docs.python.org/3/library/unittest.html
- Pytest, et populært testing rammeverk: https://pytest.org
- Artikkel om fordelene med testdrevet utvikling (TDD): https://realpython.com/test-driven-development-of-a-django-restful-api/
