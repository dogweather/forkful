---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Python: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonojen yhdistäminen on yksinkertainen tapa yhdistää kaksi tai useampia merkkijonoja yhdeksi. Ohjelmoijat käyttävät tätä toimintoa esimerkiksi tulostuksessa ja tiedostonimien luomisessa.

## Miten:
```python
nimi = "Tuomas"
ika = 24
print("Hei, olen " + nimi + " ja olen " + str(ika) + " vuotta vanha.")
````
Tulostaa:
```
Hei, olen Tuomas ja olen 24 vuotta vanha.
```

## Syvempi sukellus:
Merkkijonojen yhdistäminen ei ole uusi käsite, sitä on käytetty jo 1960-luvulta lähtien. Pythonissa on myös muita tapoja yhdistää merkkijonoja, kuten käyttämällä %-operaattoria tai `.join()`-metodia. Lisäksi on tärkeää huomata, että merkkijonoja ei voi yhdistää muiden tietotyyppien kanssa ilman muunnoksia.

## Katso myös:
- [Pythonin virallinen dokumentaatio](https://docs.python.org/3/tutorial/datastructures.html#more-on-strings)
- [Merkkijonojen yhdistämisen eri tavat Stack Overflow'ssa](https://stackoverflow.com/questions/46838697/which-is-best-among-the-two-for-concatenation-in-python)