---
title:                "Python: Poistetaan merkit, jotka vastaavat kaavaa"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Miksi Poistaa Merkkejä, Jotka Täsmäävät Kaavaan?

On monia tilanteita, joissa voi olla tarpeen poistaa merkkejä, jotka täsmäävät tiettyyn kaavaan. Tämä voi johtua esimerkiksi tietojen siivoamisesta tai tarpeesta löytää tiettyä dataa suuresta määrästä tekstejä tai tiedostoja. Onneksi Python tarjoaa helpon tavan tehdä tämä tietojen käsittely tehtävä.

## Miten Tehdä Se?

Pythonin "re" -moduuli tarjoaa erilaisia toimintoja, joilla voi toteuttaa merkkijonojen hakuja ja korvaamista kaavoilla. Alla on esimerkkejä, miten poistaa merkkejä kaavan mukaisesti tekstimuodosta ja listasta.

```Python
import re

# Poistaa kaikki numerot merkkijonosta
teksti = "Tämä on 2021 vuoden kirjoitus."
puhdas_teksti = re.sub(r'\d+', '', teksti)
print(puhdas_teksti) # Tulostaa "Tämä on vuoden kirjoitus."

# Poistaa kaikki kirjaimet "a" listasta
lista = ['auto', 'banaani', 'talo']
puhdas_lista = [re.sub('a', '', sana) for sana in lista]
print(puhdas_lista) # Tulostaa ['uto', 'bnni', 'tlo']
```

## Syvempi Sukellus

Re-moduulin avulla voi käyttää erilaisia kaavoja merkkijonojen hakuun ja korvaamiseen, kuten säännöllisiä lausekkeita. Tämä mahdollistaa tarkemman haun ja korvaamisen halutuilla kriteereillä. Esimerkiksi, jos haluat poistaa kaikki merkit paitsi kirjaimet ja numerot, voit käyttää seuraavaa käskyä: ```re.sub(r'[^a-zA-Z0-9]', '', teksti)```.

On myös mahdollista käyttää erilaisia "flags" -lippuja, kuten "re.I", joka tekee kaavasta tapauksia erottamattoman.

## Katso Myös

- [Pythonin dokumentaatiot: re-moduuli](https://docs.python.org/3/library/re.html)
- [W3Schools: Regular Expressions in Python](https://www.w3schools.com/python/python_regex.asp)
- [Real Python: Regular Expressions](https://realpython.com/regex-python/)

Kiitos lukemisesta! Toivottavasti tämä artikkeli auttoi sinua ymmärtämään, miten poistaa merkkejä, jotka täsmäävät kaavaan käyttämällä Pythonia. Muistathan, että säännölliset lausekkeet voivat olla hyödyllisiä myös monissa muissa tietojen käsittely tehtävissä.