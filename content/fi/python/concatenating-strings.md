---
title:                "Tekstijonojen yhdistäminen"
html_title:           "Python: Tekstijonojen yhdistäminen"
simple_title:         "Tekstijonojen yhdistäminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

On monia eri tilanteita, joissa ohjelmointitehtävä vaatii useiden merkkijonojen yhdistämistä yhdeksi merkkijonoksi. Tätä kutsutaan merkkijonojen yhdistämiseksi tai "concatenation" englanniksi. Tämä artikkeli kertoo miten tämä voidaan tehdä Pythonin avulla ja mikä on syvempi tausta tämän ohjelmointitekniikan takana.

## Miten

Merkkijonojen yhdistäminen Pythonilla on yksinkertaista käyttämällä "+" operaattoria. Katso alla oleva esimerkki:

```python
# Määritetään kaksi merkkijonoa
string1 = "Hei"
string2 = "maailma!"

# Yhdistetään merkkijonot ja tallennetaan uuteen muuttujaan
uusi_merkkijono = string1 + string2

# Tulostetaan uusi merkkijono
print(uusi_merkkijono)

# Output: "Hei maailma!"
```

Kuten näet, yhdistetty merkkijono tallennetaan uuteen muuttujaan ja tulostetaan sitten käyttämällä print-funktiota. On myös mahdollista yhdistää useampia merkkijonoja yhdellä kertaa käyttämällä samaa "+" operaattoria.

## Syvempi sukellus

Merkkijonojen yhdistämisellä on tärkeä rooli tietojenkäsittelyssä ja ohjelmoinnissa yleensä. Se auttaa tekemään koodista selkeämpää ja helpommin ymmärrettävää. Lisäksi Pythonilla on muita tapoja yhdistää merkkijonoja, kuten käyttämällä join()-funktiota tai f-string-syntaksia.

On myös hyvä huomata, että merkkijonojen yhdistäminen voi olla hidas prosessi tietyissä tapauksissa, koska uusi merkkijono tulee luoda jokaisen yhdistämisen yhteydessä. Tästä syystä, jos tarvitset tehokkaampaa tapaa yhdistää useita merkkijonoja, kannattaa harkita liittymäoperaattorin ("join operator") tai StringBuilderiin pohjautuvan moduulin käyttöä.

## Katso myös

- [Pythonin virallinen dokumentaatio merkkijonojen yhdistämisestä](https://docs.python.org/3/library/stdtypes.html#str.join)
- [Stack Overflow -parhaat käytännöt merkkijonojen yhdistämisestä Pythonissa](https://stackoverflow.com/questions/45258048/concatenating-two-string-in-python-most-flexible-way)
- [Pythonin virallinen ohjelmistokehittäjien yhdistys (PSF)](https://www.python.org/)