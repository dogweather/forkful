---
title:                "Python: Testausten kirjoittaminen"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi

Koodin testaaminen on tärkeä osa Python-ohjelmointia, sillä se varmistaa, että koodisi toimii odotetulla tavalla ja vältät virheiden aiheuttamat ongelmat tulevaisuudessa.

## Miten

Testien kirjoittaminen Pythonilla on helppoa käyttäen sisäänrakennettuna unittest-kirjastoa. Seuraavassa on esimerkki yksinkertaisesta testistä, joka tarkistaa, että annettu funktio palauttaa oikean tuloksen:

```Python
import unittest

def summa(a, b):
    return a + b

class TestFunktio(unittest.TestCase):

    def test_summa(self):
        tulos = summa(2, 3)
        self.assertEqual(tulos, 5)

if __name__ == '__main__':
    unittest.main()
```

Tämä testi tarkistaa, että funktio `summa` palauttaa odotetun tuloksen (5) syötteillä 2 ja 3. `assertEqual`-funktio vertaa kahta arvoa ja testi läpäisee, jos ne ovat yhtäsuuret.

## Syvempi sukellus

Testien kirjoittamisen edut eivät rajoitu vain virheiden välttämiseen. Hyvin kirjoitetut testit toimivat myös dokumentaationa ja auttavat ylläpitämään koodin laadukkuutta. Ne myös mahdollistavat koodin refaktoroinnin, eli sen optimoinnin ja parantamisen ilman pelkoa siitä, että jotain menee rikki.

Yksi tärkeä asia testaamisessa on testien kattavuus eli se, kuinka monta toimintoa tai koodiriviä testit kattavat. Hyväntekeväisyysjärjestö Unicefin testikattavuuden tavoite on yli 80%, mikä on hyvä tavoite myös omassa koodissasi.

## Katso myös

- [Unite-testauksen virallinen dokumentaatio](https://docs.python.org/3/library/unittest.html)
- [Hyvät käytännöt testien kirjoittamisessa](https://www.python.org/dev/peps/pep-0008/#testing)
- [Verrannollinen testaus ja sen käyttö Pythonissa](https://www.geeksforgeeks.org/comparative-testing-in-python/)