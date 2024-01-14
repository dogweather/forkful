---
title:                "Python: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi meidän pitäisi yhdistää merkkijonoja? Yksi yleisimmistä syistä on luoda dynaamisia viestejä tai tulosteita, jotka sisältävät muuttuvia elementtejä, kuten käyttäjän syöttämät tiedot. 

## Kuinka

Yhdistämiseen merkkijonoja Pythonissa on useita tapoja, mutta yksi yksinkertaisimmista on käyttää plus-merkkiä (+). Tässä esimerkissä lisätään kaksi merkkijonoa yhteen ja tulostetaan tulos:

```Python
merkkijono_1 = "Tämä on "
merkkijono_2 = "esimerkki"
tulos = merkkijono_1 + merkkijono_2
print(tulos)
```

Tuloste: Tämä on esimerkki

Toinen tapa yhdistää merkkijonoja on käyttää `.join()` -funktiota. Tämä toimii hyvin, kun halutaan yhdistää useampia merkkijonoja yhteen. 

```Python
merkkijonot = ["Hei", "sinä"]
tulos = " ".join(merkkijonot)
print(tulos)
```

Tuloste: Hei sinä

## Syvällisempi sukellus

Jos haluat syvällisempää tietoa merkkijonojen yhdistämisestä Pythonissa, tässä on muutamia seikkoja, jotka kannattaa huomioida:

- Merkkijonoja voi yhdistää vain samantyyppisten tietojen kanssa, eli esimerkiksi ei voi yhdistää merkkijonoa ja kokonaislukua.
- Jos haluat lisätä muuttuvia arvoja merkkijonoon, voit käyttää f-string-merkkijonoa, esimerkiksi `f"Tervetuloa {nimi}!"`.
- Muista myös, että merkkijonat ovat immuuneja, eli niitä ei voi muuttaa suoraan. Sen sijaan merkkijonon muuttaminen luo aina uuden merkkijonon.

## Katso myös

- [Pythonin merkkijonon yhdistämisen dokumentaatio](https://docs.python.org/3/library/string.html#string-concatenation)
- [Pythonin f-string-merkkijonojen opas](https://realpython.com/python-f-strings/)
- [Perusteet merkkijonojen käsittelystä Pythonissa](https://www.freecodecamp.org/news/python-string-split-concatenate-format/)