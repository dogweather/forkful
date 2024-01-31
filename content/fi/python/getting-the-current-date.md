---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:16:47.798350-07:00
simple_title:         "Nykyisen päivämäärän hankkiminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Nykyisen päivämäärän haku antaa ohjelmillesi tietoa reaaliajassa. Käyttäjille räätälöity sisältö, aikaleimat ja tehtävien ajoitus vaativat tätä.

## How to: (Kuinka tehdään:)
```Python
from datetime import date

# Haetaan tämänhetkinen päivämäärä
tanaan = date.today()

print(f"Tänään on: {tanaan}")
```
Tulostus:
```
Tänään on: 2023-04-05
```

```Python
import datetime

# Toisaalta voit käyttää datetime luokkaa saadaksesi enemmän tietoa
nyt = datetime.datetime.now()

print(f"Nyt: {nyt}")
```
Tulostus:
```
Nyt: 2023-04-05 17:45:01.762130
```

## Deep Dive (Sukellus syvyyksiin):
Pythonin `datetime` moduuli on standardikirjaston osa. Se on tärkeä, koska päivämäärät ja ajat ovat ohjelmistokehityksessä kaikkialla. `datetime` ilmaantui Pythoniin version 2.3 myötä. Vaihtoehtoinen työkalu on `time` moduuli, mutta `datetime` on suositumpi, koska se käsittelee sekä päivämääriä että kellonaikoja ja on objektiivisempi. Implementaation yksityiskohta: `date.today()` palauttaa `date` objektin, joka sisältää nykyisen paikallisen päivämäärän, kun taas `datetime.datetime.now()` antaa `datetime` objektin, joka sisältää nykyhetken paikallisen päivämäärän ja kellonajan.

## See Also (Katso myös):
- Pythonin virallinen dokumentaatio `datetime` moduulista: https://docs.python.org/3/library/datetime.html
- Pytz-kirjaston käyttö aikavyöhykkeiden kanssa: https://pypi.org/project/pytz/
- dateutil-kirjaston laajennetut toiminnot päivämäärän käsittelyyn: https://dateutil.readthedocs.io/en/stable/
