---
title:                "Python: Merkkijonon kirjoittainen suurentaminen"
simple_title:         "Merkkijonon kirjoittainen suurentaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi: Miksi pitäisi päätyä muuttamaan merkkijonon ensimmäinen kirjain isoksi?

Merkkijonon käsittely on tärkeä osa Python-ohjelmointia. Yksi yleinen tehtävä on muuttaa merkkijonon ensimmäinen kirjain isoksi. Tämä voi olla hyödyllistä esimerkiksi silloin, kun halutaan muuttaa käyttäjän syöttämät tiedot yhdenmukaiseen muotoon. Tässä blogikirjoituksessa käymme läpi, miten tämä tehtävä voidaan suorittaa helposti Pythonilla.

## Kuinka: Esimerkkejä koodista ja tulosteista

Tekstin muuttaminen isoksi on yksinkertaista käyttämällä sisäänrakennettua `capitalize()` -funktiota. Tämä funktio muuttaa merkkijonon ensimmäisen kirjaimen isoksi ja palauttaa uuden merkkijonon. Alla olevassa esimerkissä käytämme tätä funktiota string-muuttujan kanssa.

```Python
string = "tervetuloa!"

print(string.capitalize())
```

Tulostus: "Tervetuloa!"

Jos haluamme muuttaa vain ensimmäisen kirjaimen, voimme käyttää myös `title()` -funktiota.

```Python
string = "tervetuloa!"

print(string.title())
```

Tulostus: "Tervetuloa!"

Molemmat funktiot toimivat myös silloin, kun merkkijonossa on monta sanaa.

```Python
string = "hei kaikki yhdessä!"

print(string.capitalize())
print(string.title())
```

Tulostus: "Hei kaikki yhdessä!" "Hei Kaikki Yhdessä!"

## Syvällinen sukellus: Tietoa merkkijonojen käsittelystä

Merkkijonoilla on useita sisäänrakennettuja metodeja ja funktioita, joiden avulla niitä voidaan muokata ja käsitellä. Käyttämämme `capitalize()` ja `title()` ovat vain muutamia esimerkkejä näistä. On myös tärkeää muistaa, että merkkijonot ovat usein muuttumattomia, eli niitä ei voi muuttaa suoraan vaan ne pitää aina tallentaa uuteen muuttujaan.

## Katso myös

- Pythonin merkkijonojen käsittely: https://realpython.com/python-strings/
- Pythonin sisäänrakennetut merkkijonofunktiot: https://docs.python.org/3/library/stdtypes.html#string-methods