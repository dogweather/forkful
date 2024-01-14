---
title:    "Python: Merkkijonon pituuden löytäminen"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit selvittää merkkijonon pituuden? Yksi syy voisi olla tietääkseen kuinka monta merkkiä tarvitset esimerkiksi laskurien tai taulukoiden määrittämiseen.

## Miten

Merkkijonon pituuden selvittäminen Pythonissa on yksinkertaista käyttämällä `len()` funktiota. Tämä funktio palauttaa merkkijonon pituuden numeromuodossa.

```Python
merkkijono = "Tämä on esimerkkimerkkijono"
print(len(merkkijono))
```
Tämän koodin tuloste on `26`, mikä vastaa merkkijonon pituutta.

## Syvemmälle

Pythonissa merkkijonojen pituuden selvittäminen perustuu merkkijonojen ominaisuuteen, nimeltään **len**. Tämä ominaisuus löytyy kaikista merkkijonoista ja sen arvo on merkkijonon merkkien lukumäärä. `len()` funktiolla pääset käsiksi tähän ominaisuuteen ja voit käyttää sitä laskemaan merkkijonon pituuden.

On myös hyvä huomata, että `len()` toimii myös muiden tietotyyppien, kuten listojen ja tuplejen, kanssa. Se palauttaa niiden alkioiden lukumäärän.

## Katso myös

* [Pythonin virallinen dokumentaatio merkkijonojen pituudesta](https://docs.python.org/3/library/stdtypes.html#str.__len__)

* [Stack Overflow vastaus, jossa selitetään miten `len()` toimii](https://stackoverflow.com/questions/32129576/how-does-len-work)

* [Merkkijonojen käsittely Pythonissa – opetusohjelma](https://realpython.com/python-strings/)