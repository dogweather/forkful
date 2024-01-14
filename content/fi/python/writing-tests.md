---
title:    "Python: Testien kirjoittaminen"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Miksi: Miksi kirjoittaa testejä?

Kirjoittaminen testejä on tärkeä osa hyvää ohjelmistokehitystä. Se auttaa varmistamaan, että koodi toimii oikein ja pysyy virheettömänä muutosten tai päivitysten jälkeen. Lisäksi testien avulla voit nopeasti havaita mahdolliset ongelmat ja korjata ne ennen kuin ne päätyvät loppukäyttäjän käsiin.

## Kuinka tehdä: Koodin esimerkit ja tulosteet

Pythonissa voit helposti kirjoittaa yksikkötestejä käyttämällä sisäänrakennettua unittest-moduulia. Tässä on esimerkki luokasta, joka sisältää yksinkertaisen funktion ja testit sen testaamiseksi:

```Python
class Laskin:
    def summa(self, x, y):
        return x + y

import unittest

class TestLaskin(unittest.TestCase):
    def setUp(self):
        self.laskin = Laskin()

    def test_summa(self):
        tulos = self.laskin.summa(2, 3)
        odotettu_tulos = 5
        self.assertEqual(tulos, odotettu_tulos)

    def test_summa_negatiivisilla(self):
        tulos = self.laskin.summa(-5, 10)
        odotettu_tulos = 5
        self.assertEqual(tulos, odotettu_tulos)

if __name__ == '__main__':
    unittest.main()
```

Tulos on seuraava:

```terminal
..
----------------------------------------------------------------------
Ran 2 tests in 0.000s

OK
```

Näemme, että molemmat testit läpäisivät ja koodimme toimii odotetusti. Jos nyt muutamme funktion summa niin, että se palauttaa x - y, testit epäonnistuvat ja saamme seuraavan tulosteen:

```terminal
.F
======================================================================
FAIL: test_summa_negatiivisilla (__main__.TestLaskin)
----------------------------------------------------------------------
Traceback (most recent call last):
  File "test_laskin.py", line 14, in test_summa_negatiivisilla
    self.assertEqual(tulos, odotettu_tulos)
AssertionError: 15 != 5

----------------------------------------------------------------------
Ran 2 tests in 0.001s

FAILED (failures=1)
```

Tämä osoittaa, että testit todella testaavat koodiamme ja auttavat meitä löytämään mahdollisia virheitä.

## Syvemmälle: Lisätietoa testeistä

On olemassa erilaisia testaustyyppejä, kuten yksikkö-, integraatio- ja hyväksymistestit. Yksikkötestit testaavat yksittäistä osaa ohjelmasta, kun taas integraatiotestit testaavat useamman osan yhteistoimintaa. Hyväksymistestit, kuten käyttöliittymätestit, testaavat koko sovelluksen toiminnallisuuksia.

Testien kirjoittaminen myös auttaa suunnittelemaan koodia, koska sinun täytyy ottaa huomioon kaikki mahdolliset skenaariot ja reunatapaukset. Lisäksi testien avulla voit nopeasti varmistaa, että uudet muutokset eivät riko toimivaa koodia.

## Katso myös

- [Python unittest dokumentaatio](https://docs.python.org/3/library/unittest.html)
- [TDD (Test Driven Development) opas](https://www.obeythetestinggoat.com/pages/book.html#toc)
- [Flask testaus opas](https://flask.palletsprojects.com/en/1.1.x/testing/)