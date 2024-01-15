---
title:                "Pienentäminen merkkijonoksi"
html_title:           "Python: Pienentäminen merkkijonoksi"
simple_title:         "Pienentäminen merkkijonoksi"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon ensimmäisen kirjaimen isoksi alkukirjaimeksi? Tämä on hyödyllistä esimerkiksi silloin, kun haluat muuttaa käyttäjän syöttämän merkkijonon oikeaan muotoon ennen sen tallentamista tietokantaan tai käytä sitä vertailussa jonkun toisen merkkijonon kanssa.

## Kuinka tehdä

```Python
# Esimerkki koodi
string = "tervehdys maailma!"
print(string.capitalize())
```

Tässä esimerkissä annettu merkkijono "tervehdys maailma!" muutetaan isoksi alkukirjaimeksi käyttäen capitalize()-funktiota. Koodin tulostus on "Tervehdys maailma!", jossa ensimmäinen kirjain on nyt isolla alkukirjaimella.

## Syventävä tieto

Capitalize()-funktio on osa Pythonin sisäänrakennettua "string" -moduulia ja se voidaan käyttää kaikissa merkkijonoissa. Jokaisen sanan ensimmäinen kirjain muuttuu isoksi, mutta muut kirjaimet pysyvät ennallaan. Tämä toiminto on hyödyllinen myös silloin, kun halutaan päästä eroon mahdollisista kirjoitusvirheistä merkkijonossa, kuten jos käyttäjä on syöttänyt ensimmäisen kirjaimen vahingossa pienellä.

## Katso myös

- [Python string -moduuli](https://docs.python.org/3/library/string.html)
- [Merkkijonojen käsittely Pythonissa](https://realpython.com/python-strings/)
- [Pythonin järjestelmävalmiudet](https://docs.python.org/3/library/sys.html)