---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"

category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä & Miksi?
Testikoodaus varmistaa ohjelmistosi laadun. Sen avulla havaitset virheet ajoissa ja varmistat, että koodisi tekee sen, mitä sen on tarkoitus.

## How to: - Miten:
```Python
import unittest

# Testattava funktio
def summa(a, b):
    return a + b

# Testiluokka, joka perii unittest.TestCase
class SummaTestit(unittest.TestCase):
    def test_summa_positiiviset(self):
        self.assertEqual(summa(2, 3), 5)

    def test_summa_negatiiviset(self):
        self.assertEqual(summa(-2, -3), -5)
        
    def test_summa_nollan_kanssa(self):
        self.assertEqual(summa(0, 0), 0)

# Testien suoritus
if __name__ == '__main__':
    unittest.main()
```

Näin testataan funktiota `summa`. Saatavilla olevan terminaalin tuloste voi näyttää tältä:
```
...
----------------------------------------------------------------------
Ran 3 tests in 0.001s

OK
```

## Deep Dive - Syväsukellus
Testit kehittyivät osaksi ohjelmointia jo 1960-luvulla. Nörtit ovat sittemmin luoneet useita testauskehyksiä, kuten PyUnit Pythonille. PyUnit tukee useita testaustyylejä, mukaan lukien yksikkötestaus ja integraatiotestaus; se on osa Pythonin standardikirjastoa `unittest`-nimellä. Vaihtoehtoja kuten pytest ja nose2 ovat myös suosittuja, tarjoten erilaisia ominaisuuksia ja yksinkertaistaen testien kirjoittamista.

## See Also - Katso Myös
- Pythonin virallinen `unittest`-ohje: https://docs.python.org/3/library/unittest.html
- pytest-dokumentaatio: https://docs.pytest.org/en/stable/
- Testauskäytännöt Pythonin virallisella oppimissivustolla: https://realpython.com/python-testing/
