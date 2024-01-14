---
title:                "Python: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-tests.md"
---

{{< edit_this_page >}}

# Miksi kirjoittaa testeja Pythonilla?

On monia syitä, miksi kirjoittaa testeja Pythonilla. Yksi tärkeimmistä on varmistaa, että koodi toimii odotetusti ja pysyy virheettömänä jopa muutosten ja päivitysten jälkeen. Lisäksi testien avulla voidaan nopeasti havaita ja korjata mahdolliset bugsit tai ongelmat koodissa.

## Kuinka kirjoittaa testeja Pythonilla

Pythonilla testien kirjoittaminen on helppoa ja suoraviivaista. Yksi yleisesti käytetty kirjasto testeihin on unittest, joka tarjoaa monipuoliset ominaisuudet testejen kirjoittamiseen ja suorittamiseen.

Seuraavassa on esimerkki yksinkertaisesta testistä, joka tarkistaa, onko kahden luvun summa oikein:

```Python
import unittest

def summa(x, y):
    return x + y

class TestSumma(unittest.TestCase):

    def test_summa(self):
        tulos = summa(3, 5)
        self.assertEqual(tulos, 8)

if __name__ == '__main__':
    unittest.main()
```

Koodin suorittamisen jälkeen näemme, että testi on läpäisty, koska kahden luvun summa oli odotetusti 8. Jos koodiin tehdään muutoksia ja testi epäonnistuu, voimme nopeasti havaita virheen ja korjata sen ennen kuin se aiheuttaa ongelmia.

## Syvennys testeihin kirjoittamiseen

Testien kirjoittaminen voi tuntua aikaa vievältä, mutta se säästää paljon vaivaa ja aikaa pitkällä aikavälillä. On myös hyvä noudattaa testien kirjoittamista ensin -periaatetta, eli kirjoittaa testit ennen varsinaista koodia. Tämä auttaa varmistamaan, että koodi suorittaa halutun toiminnon ja että kaikki reunaehtojen ja virhetilanteiden käsittely on huomioitu.

Lisäksi testejä kannattaa kirjoittaa mahdollisimman laajasti erilaisille syötteille ja reunaehtoihin, jotta kattavuus olisi mahdollisimman korkea. Testien suorittaminen säännöllisesti myös varmistaa, että koodi pysyy virheettömänä muutosten ja päivitysten jälkeen.

# Katso myös

- [UnitTest-dokumentaatio](https://docs.python.org/3/library/unittest.html)
- [Python-testin kirjoittaminen 60 sekunnissa](https://www.freecodecamp.org/news/unit-testing-python-tutorial/)
- [Test-Driven Development (TDD)](https://codeburst.io/understanding-test-driven-development-behavior-driven-development-using-unit-tests-f6e82441099b)