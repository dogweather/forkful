---
aliases:
- /nl/python/writing-tests/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:17.675957-07:00
description: "Testen schrijven betekent code maken om te controleren of andere code\
  \ goed werkt. We doen dit om bugs te vangen, betrouwbaarheid te garanderen en updates\u2026"
lastmod: 2024-02-18 23:09:01.437722
model: gpt-4-0125-preview
summary: "Testen schrijven betekent code maken om te controleren of andere code goed\
  \ werkt. We doen dit om bugs te vangen, betrouwbaarheid te garanderen en updates\u2026"
title: Tests Schrijven
---

{{< edit_this_page >}}

## Wat & Waarom?

Testen schrijven betekent code maken om te controleren of andere code goed werkt. We doen dit om bugs te vangen, betrouwbaarheid te garanderen en updates minder eng te maken.

## Hoe te:

Laten we Pythons ingebouwde `unittest` framework gebruiken.

```Python
import unittest

def optellen(a, b):
    return a + b

class TestOptellenFunctie(unittest.TestCase):
    def test_optellen_gehele_getallen(self):
        self.assertEqual(optellen(1, 2), 3)

    def test_optellen_tekstreeksen(self):
        self.assertEqual(optellen('abc', 'def'), 'abcdef')

if __name__ == '__main__':
    unittest.main()
```

Voer het uit, je zult iets zien als:

```
..
----------------------------------------------------------------------
Ran 2 tests in 0.001s

OK
```

Twee puntjes betekenen dat twee tests geslaagd zijn. Alles is in orde.

## Diepere Duik

Python testing begon groot te worden met `unittest` (geïnspireerd door Java's JUnit). Nu zijn er `pytest` en `nose`, modernere tools met een eenvoudigere syntaxis en betere functies. Wanneer je tests schrijft, onthoud: isoleer testgevallen, test grensgevallen en simuleer externe afhankelijkheden om je te focussen op de logica van je code, niet op de buitenwereld.

## Zie Ook

Verdiep je verder in testen met deze bronnen:

- Python's `unittest` documentatie: https://docs.python.org/3/library/unittest.html
- `pytest` voor een modernere aanpak: https://docs.pytest.org/en/latest/
- Simuleren in tests met `unittest.mock`: https://docs.python.org/3/library/unittest.mock.html
