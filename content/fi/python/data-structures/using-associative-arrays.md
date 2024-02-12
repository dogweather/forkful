---
title:                "Assosiatiivisten taulukoiden käyttö"
aliases:
- /fi/python/using-associative-arrays/
date:                  2024-01-30T19:12:51.863109-07:00
model:                 gpt-4-0125-preview
simple_title:         "Assosiatiivisten taulukoiden käyttö"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Associatiiviset taulukot, Pythonissa tunnettu nimellä sanakirjat, map-paavat avaimet arvoihin, mikä tekee datan hakemisesta, muokkaamisesta tai seuraamisesta uniikin tunnisteen avulla helppoa. Ohjelmoijat käyttävät niitä niiden tehokkuuden vuoksi elementtien haussa ja niiden joustavuuden vuoksi monimutkaisten datarakenteiden esittämisessä.

## Miten:

Sanakirjan luominen Pythonissa on suoraviivaista. Sijoitat avain-arvo -pareja aaltosulkuihin `{}`, avainten ja arvojen ollessa erotettu kaksoispisteellä:

```Python
# Luo associatiivinen taulukko (sanakirja)
my_dict = {"name": "John", "age": 30, "city": "New York"}
print(my_dict)
```

Tuloste:
```
{'name': 'John', 'age': 30, 'city': 'New York'}
```

Arvon hakeminen avaimella on yksinkertaista:

```Python
# Hae arvo
print(my_dict["name"])
```

Tuloste:
```
John
```

Elementtien lisääminen tai päivittäminen tehdään antamalla arvo avaimelle:

```Python
# Lisää uusi avain-arvopari
my_dict["email"] = "john@example.com"
# Päivitä arvo
my_dict["age"] = 31
print(my_dict)
```

Tuloste:
```
{'name': 'John', 'age': 31, 'city': 'New York', 'email': 'john@example.com'}
```

Iteroidaksesi sanakirjan läpi:

```Python
# Iteroi läpi avain-arvoparit
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

Tuloste:
```
name: John
age: 31
city: New York
email: john@example.com
```

## Syventävä tarkastelu

Pythonin associatiiviset taulukot, eli sanakirjat, otettiin käyttöön tarjoamaan datarakennetta tehokkaaseen datan hakuun ja manipulointiin. Toisin kuin sekvenssit, jotka on indeksoitu numeroiden avulla, sanakirjat indeksoidaan avainten mukaan, jotka voivat olla mitä tahansa muuttumatonta tyyppiä. Tämä suunnitteluvalinta tekee sanakirjoista ihanteellisesti soveltuvia nopeille haku taulukoille, joissa avaimet map-paavat uniikkeihin arvoihin.

Historiallisesti Pythonin sanakirjat on toteutettu käyttäen hajautustaulua, varmistaen, että keskimääräinen aikavaativuus haku-, lisäys- ja poisto-operaatioissa on O(1). Python 3.6:sta ja sitä myöhemmistä versioista lähtien, sanakirjat ylläpitävät myös elementtien lisäysjärjestystä, yhdistäen hajautustaulujen edut lisäysjärjestyksen ennustettavuuteen, jota nähdään järjestetyissä datarakenteissa.

Vaikka sanakirjat ovat uskomattoman monipuolisia, joissakin erityistapauksissa vaihtoehdot kuten `collections.defaultdict` tai `collections.OrderedDict` (ennen Python 3.7) saattavat olla sopivampia. `defaultdict` on erityisen hyödyllinen, kun tarvitset sanakirjan palauttavan oletusarvon olemattomille avaimille, yksinkertaistaen tietynlaisia ehdollisia loogisia operaatioita. Kuitenkin, jatkuvan parannuksen ja Pythonin kehityksen myötä, sisäänrakennettu sanakirjaluokka pysyy usein valintana associatiivisiin taulukkoihin sen robustiuden ja laatikon ulkopuolelta tarjotun mukavuuden vuoksi.
