---
title:    "Python: Testausten kirjoittaminen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi: Testien kirjoittamisen merkitys Python-ohjelmoinnissa

Kirjoittaminen testejä on olennainen osa Python-ohjelmointia, ja se auttaa varmistamaan koodin toimivuuden ja vähentää virheiden määrää. Testien avulla voi myös helposti tunnistaa ja korjata mahdollisia ongelmia, joka säästää aikaa ja vaivaa pitkällä tähtäimellä.

## Miten: Esimerkkejä testien kirjoittamisesta ja tulostuksista

```Python
# Aluksi tuodaan unittest-kirjasto

import unittest

# Luodaan funktio, joka palauttaa kahden numeron summan

def summa(a, b):
  return a + b

# Tässä testataan, että funktio palauttaa oikean tuloksen kahdelle syötetylle numerolle

class TestSumma(unittest.TestCase):
  
  def test_summa(self):
    self.assertEqual(summa(2, 3), 5) # AssertEqual tarkistaa, että palautusarvo on yhtäsuuri kuin määritelty arvo
    
# Suoritetaan testi

if __name__ == '__main__':
  unittest.main()

# Jos testi läpäisee, näytetään seuraava tulos:

# ----------------------------------------------------------------------
# Luokka TestSumma
# ----------------------------------------------------------------------
# test_summa saa arvoksi 5.

# ----------------------------------------------------------------------
# Ran 1 test in 0.000s

# OK

```

## Syvempää sukellusta: Tietoa testien kirjoittamisesta

Testien kirjoittaminen tuo mukanaan monia etuja Python-ohjelmoinnissa. Ensinnäkin, testien ansiosta voimme olla luottavaisia koodin toimivuuteen ja vähentää virheiden määrää. Tämä säästää aikaa ja vaivaa pitkällä tähtäimellä, koska ongelmien löytäminen ja korjaaminen myöhemmin voi olla vaikeampaa ja aikaavievämpää. Lisäksi testit toimivat dokumentaationa koodin toiminnasta ja mahdollistavat nopean ja tehokkaan kehitysprosessin.

## Katso myös

- [Unittest-dokumentaatio](https://docs.python.org/3/library/unittest.html)
- [Pythonin virallinen sivusto](https://www.python.org/)
- [Get started with testing in Python](https://realpython.com/python-testing/) (englanniksi)