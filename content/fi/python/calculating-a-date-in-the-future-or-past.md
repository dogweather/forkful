---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Python: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Lasketaan tulevaisuuden tai menneisyyden päivämäärää tarkoittaa tietyn ajanjakson lisäämistä tai vähentämistä tietystä päivämäärästä. Ohjelmoijat tekevät näin parisyistä, kuten tietokannan vanhentuneiden tietueiden puhdistamiseksi tai tapahtumien ajoitukseksi tulevaisuudessa.

## Kuinka:

Alla on yksinkertainen Python-koodi, joka näyttää, kuinka laskea tulevaisuuden päivämäärä.

```Python
from datetime import datetime, timedelta

# nykyinen päivämäärä ja aika
nyt = datetime.now()

# lisätään 5 päivää  nykyisestä päivämäärästä
tulevaisuus = nyt + timedelta(days=5)

print('Nykyinen päivämäärä:', nyt)
print('5 päivää myöhemmin:', tulevaisuus)
```
Tämä on esimerkkitulo:

```Python
Nykyinen päivämäärä: 2022-04-24 17:53:07.258743
5 päivää myöhemmin: 2022-04-29 17:53:07.258743
```

## Syvä sukellus:

Historiallisesti päivämäärän laskeminen tulevaisuudessa tai menneisyydessä oli paljon monimutkaisempaa, koska se vaati päivämäärä- ja aikavuoksiin liittyvien sääntöjen, kuten karkausvuosien, huomioon ottamista. Pythonin datetime-moduuli tekee tämän paljon helpommaksi, koska se hoitaa kaikki nämä yksityiskohdat automaattisesti.

Vaihtoehtoisesti, voit käyttää dateutil-moduulia, joka tarjoaa monipuolisemmat toiminnot kuten toistuvien ajanjaksojen laskeminen.

Vaikka Pythonin datetime-moduuli käsittelee monimutkaisia yksityiskohtia, sen takana on huomattavan paljon tietojenkäsittelyä. Esimerkiksi, karkausvuodet otetaan huomioon määrittämällä, ovatko vuodet jaollisia 4:llä, 100:lla ja 400:lla.

## Katso myös:

Pythonin virallinen dokumentaatio tarjoaa kattavat tiedot datetime-moduulista:
https://docs.python.org/3/library/datetime.html

Kattava opas Pythonin dateutil-moduulin käyttämiseen:
https://dateutil.readthedocs.io/en/stable/