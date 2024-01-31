---
title:                "Päivämäärän muuntaminen merkkijonoksi"
date:                  2024-01-20T17:37:15.309715-07:00
model:                 gpt-4-1106-preview
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Muunnetaan päivämäärä merkkijonoksi, jotta voimme näyttää päivämäärän haluamassamme muodossa. Sitä tarvitaan lokitiedostoissa, käyttöliittymissä ja datan tallennuksessa.

## How to: (Kuinka tehdä:)
```Python
from datetime import datetime

# Nykyinen aika ja päivämäärä
nyt = datetime.now()

# Muotoilu merkkijonoksi
merkkijono_muodossa = nyt.strftime("%d.%m.%Y %H:%M")

print(merkkijono_muodossa)  # Esim. "30.03.2023 16:41"
```

## Deep Dive (Syväsukellus)
Pythonissa päivämäärät muunnetaan merkkijonoksi `datetime`-moduulilla, joka tuli käyttöön Python 2.3:ssa. Vaihtoehtoiset kirjastot, kuten `Arrow` tai `Pendulum`, tarjoavat enemmän ominaisuuksia mutta eivät ole vakiokirjastossa. `strftime`-metodi on vakiintunut tapa päivämäärän muotoiluun merkkijonoksi, jossa voit määritellä päivämäärän esitysmuodon monipuolisesti.

## See Also (Katso Myös)
- Pythonin virallinen dokumentaatio `datetime`: https://docs.python.org/3/library/datetime.html
- strftime()- ja strptime()-käytännön ohjeet: https://strftime.org/
- Arrow-dokumentaatio: https://arrow.readthedocs.io/en/latest/
- Pendulum-dokumentaatio: https://pendulum.eustace.io/
