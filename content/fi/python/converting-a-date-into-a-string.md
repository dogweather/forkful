---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Python: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Päivämäärän muuntaminen merkkijonoksi voi olla hyödyllistä, kun halutaan esittää päivämäärä tiettyyn muotoon tai tallentaa se tiedostoon.

## Kuinka tehdä

Mikä tahansa kelvollinen päivämäärä voidaan muuntaa merkkijonoksi käyttämällä `strftime()` -funktiota. Se ottaa kaksi argumenttia: formaatin, jossa haluat päivämäärän esittää, ja päivämäärän itsessään. Alla on esimerkkejä erilaisista päivämäärän muotoiluista ja niiden tulosteista:

```Python
import datetime

# Päivämäärä merkkijonona
date = datetime.date(2021, 11, 30)
date_string = date.strftime("%d/%m/%y")
print(date_string) # Tulostaa: 30/11/21

# Viikonpäivä suomeksi
weekday = date.strftime("%A")
print(weekday) # Tulostaa: maanantai

# Päivämäärä ja kellonaika
current_time = datetime.datetime.now()
print(current_time.strftime("%d/%m/%y %H:%M:%S")) # Tulostaa esim: 30/11/21 14:30:22
```

## Syvemmälle

Päivämäärän muotoilu riippuu käytetyn formaatin kirjainlyhenteistä. Alla on taulukko yleisimmistä käytetyistä lyhenteistä ja niiden merkitykset:

| Kirjain | Merkitys               |
|---------|------------------------|
| %a      | Lyhyt päivän nimi      |
| %A      | Päivän nimi kokonaisena|
| %d      | Päivä (esim. 01)       |
| %m      | Kuukausi (esim. 11)    |
| %y      | Vuosi (2 numeroa)      | 
| %Y      | Vuosi (4 numeroa)      |
| %H      | Tunti (24-tuntijärjestelmä) |
| %M      | Minuutit               |
| %S      | Sekunnit               |

Päivämäärän muotoilussa voi myös käyttää erikoismerkkejä kuten välilyöntiä, pisteitä tai yhdysviivaa, jotka näkyvät merkkijonossa sellaisenaan.

## Katso myös

- [Pythonin virallinen dokumentaatio päivämäärän muotoilusta](https://docs.python.org/fi/3/library/datetime.html)