---
title:                "Python: Merkkijonojen yhdistäminen"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit yhdistää merkkijonoja Python-ohjelmoinnissa? Yksinkertaisesti sanottuna, se on tapa yhdistää useita merkkijonoja yhdeksi isoksi merkkijonoksi. Tämä voi olla hyödyllistä esimerkiksi tekstinmuokkauksessa tai datan käsittelyssä.

## Kuinka tehdä

Pythonissa merkkijonojen yhdistäminen tehdään helposti käyttämällä "+" -merkkiä. Tässä on esimerkki koodista ja tulosteesta:

```Python
# Luodaan kaksi erillistä merkkijonoa
merkkijono1 = "Tämä on "
merkkijono2 = "Python-ohjelmointia."

# Yhdistetään merkkijonot ja tulostetaan tulos
print(merkkijono1 + merkkijono2)

# Tulostus: Tämä on Python-ohjelmointia.
```

Yksi asia, joka kannattaa muistaa, on että merkkijonojen yhdistäminen luo aina uuden merkkijonon, eikä muuta alkuperäisiä merkkijonoja.

## Syvempi sukellus

Merkkijonojen yhdistäminen ei ole pelkästään "+" -merkin käyttöä. Pythonissa on myös mahdollista käyttää f-merkkijonoita, jotka ovat käteviä esimerkiksi muuttujien sisällyttämisessä merkkijonoon. Tässä on esimerkki koodista ja tulosteesta:

```Python
# Määritetään muuttuja
nimi = "Maija"

# Käytetään f-merkkijonoa, jossa muuttuja sisällytetään merkkijonoon
print(f"Hei, olen {nimi}.")

# Tulostus: Hei, olen Maija.
```

Lisäksi Pythonissa on myös muita tapoja yhdistää merkkijonoja, kuten käyttämällä "".join() -funktiota tai %-operaattoria. Kannattaa tutustua kaikkiin vaihtoehtoihin ja valita itselleen sopivin tapa.

## Katso myös

- [Pythonin virallinen dokumentaatio merkkijonojen yhdistämisestä](https://docs.python.org/3/library/stdtypes.html#str)
- [Pythonin merkkijonojen käsittelyä käsittelevä opetusohjelma](https://www.digitalocean.com/community/tutorials/how-to-index-and-slice-strings-in-python-3)
- [Video-opetusohjelma: Merkkijonojen käsittely Pythonissa](https://www.youtube.com/watch?v=rfscVS0vtbw)