---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Python: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Säännölliset lausekkeet ovat tapa ilmaista ja hakea kuvioita merkkijonoista. Ohjelmoijat käyttävät niitä erilaisten tietojen käsittelyyn, kuten käyttäjän syötteiden validoimiseen ja tekstin etsimiseen suurista tietokannoista.

## Kuinka tehdä:

Pythonilla säännöllisten lausekkeiden käyttö on helppoa. Alla on esimerkki koodista, jossa etsitään kaikki puhelinnumeroita tekstimuodossa olevasta tiedostosta ja tulostetaan ne.

```Python
import re

# Määritetään hakukuvio säännöllisen lausekkeen avulla
pattern = r"\d{3}-\d{3}-\d{4}"

# Luetaan tiedosto ja etsitään kaikki vastaavuudet hakukuvion mukaan
with open('tiedosto.txt', 'r') as file:
    data = file.read()
    matches = re.findall(pattern, data)

# Tulostetaan löydetyt puhelinnumerot
for match in matches:
    print(match)
```

Tämän esimerkin avulla voit soveltaa säännöllisiä lausekkeita myös omassa koodissasi.

## Syvempi sukellus:

Säännölliset lausekkeet ovat olleet käytössä jo yli 70 vuotta ja ne ovat olennainen osa monia ohjelmointikieliä, kuten Pythonia. Vaikka niiden käyttöä voidaan korvata esimerkiksi silmukoiden avulla, säännölliset lausekkeet tarjoavat tehokkaamman ja monipuolisemman tavan kuvioihin perustuvassa tiedonkäsittelyssä.

## Katso myös:

Tutustu lisää säännöllisiin lausekkeisiin ja niiden käyttöön Pythonissa Dokumentaatiosta: https://docs.python.org/3/library/re.html