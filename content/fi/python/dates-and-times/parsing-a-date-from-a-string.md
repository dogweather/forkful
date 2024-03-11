---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:08.905973-07:00
description: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta tarkoittaa\
  \ tekstuaalisen p\xE4iv\xE4- ja aikatiedon muuntamista datetime-objektiksi tai vastaavaksi\
  \ rakenteelliseksi\u2026"
lastmod: '2024-03-11T00:14:30.079506-06:00'
model: gpt-4-0125-preview
summary: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta tarkoittaa tekstuaalisen\
  \ p\xE4iv\xE4- ja aikatiedon muuntamista datetime-objektiksi tai vastaavaksi rakenteelliseksi\u2026"
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Päivämäärän jäsentäminen merkkijonosta tarkoittaa tekstuaalisen päivä- ja aikatiedon muuntamista datetime-objektiksi tai vastaavaksi rakenteelliseksi muodoksi. Tämä suoritetaan yleensä mahdollistamaan päivämäärälaskennat, vertailut ja muotoilutoiminnot tavalla, joka on kieli- ja alueagnostista. Ohjelmoijat tekevät näin tehokkaasti käsitelläkseen ja manipuloidakseen aikatietoja, jotka on poimittu lokeista, käyttäjän syötteistä tai ulkoisista lähteistä.

## Kuinka:
Pythonin vakio-/standardikirjasto tarjoaa `datetime`-moduulin, joka sisältää `strptime`-metodin tähän tarkoitukseen. Metodi vaatii kaksi argumenttia: päivämäärämerkkijonon ja muoto-ohjeen, joka määrittää syötemerkkijonon kaavan.

```python
from datetime import datetime

# Esimerkkimerkkijono
date_string = "2023-04-01 14:30:00"
# Jäsentäminen merkkijonosta datetime-objektiksi
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# Tuloste: 2023-04-01 14:30:00
```

Monipuolisempaa päivämäärän jäsentämistä varten, erityisesti kun käsitellään useita muotoja tai kulttuureja, kolmannen osapuolen kirjasto `dateutil` voi olla erittäin hyödyllinen. Se tarjoaa parser-moduulin, joka osaa jäsentää päivämääriä lähes missä tahansa merkkijonomuodossa.

```python
from dateutil import parser

# Esimerkkimerkkijonot
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1st April 2023 14:30"

# Käyttäen dateutilin parseria
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# Tuloste: 2023-04-01 14:30:00
print(parsed_date2)
# Tuloste: 2023-04-01 14:30:00
```

`dateutil` pystyy käsittelemään useimmat päivämäärämuodot ilman eksplisiittisiä muotoilumerkkijonoja, mikä tekee siitä monipuolisen valinnan sovelluksiin, jotka käsittelevät monenlaisia päivämääräesityksiä.
