---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "Kuinka: Python tekee p\xE4iv\xE4m\xE4\xE4rien muuntamisen merkkijonoiksi\
  \ helpoksi. K\xE4yt\xE4 [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-\u2026"
lastmod: '2024-04-04T02:02:47.596668-06:00'
model: gpt-4-0125-preview
summary: "Python tekee p\xE4iv\xE4m\xE4\xE4rien muuntamisen merkkijonoiksi helpoksi."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

## Kuinka:
Python tekee päivämäärien muuntamisen merkkijonoiksi helpoksi. Käytä [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) -metodia, joka on saatavilla [date](https://docs.python.org/3/library/datetime.html#date-objects) -objekteille. Tässä miten:

```Python
from datetime import datetime

# Hae nykyinen päivä ja aika
now = datetime.now()

# Muunna se merkkijonoksi muodossa: Kuukausi päivä, Vuosi
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Tuloste: maaliskuu 29, 2023 (tai nykyinen päivä)

# Muoto: VVVV-KK-PP
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Tuloste: 2023-03-29 (tai nykyinen päivä)
```


### Kuinka minä teen sen

Näin saan [ISO 8601](https://www.w3.org/QA/Tips/iso-date) -muotoisen päivämäärän aikavyöhyketiedon kanssa:

```python
def datestamp() -> str:
    """ 
    Nykyinen päivämäärä ja aika aikavyöhykkeellä ISO-muodossa.
    """
    return datetime.now().astimezone().isoformat()
```

#### Esimerkkituloste:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## Syväsukellus
Historiallisesti päivämäärän merkkijonomuunnos on ollut ohjelmoinnissa vakio tarpeen mukaan esittää päivämääriä ihmisen luettavassa muodossa. 

Vaihtoehtoja `strftime`-metodille ovat esimerkiksi `isoformat`-metodin käyttäminen ISO 8601 -muodon saavuttamiseen, tai kolmannen osapuolen kirjastot kuten `arrow` ja `dateutil`, jotka tarjoavat joustavampia jäsentämis- ja muotoiluvaihtoehtoja.

Toteutuksen kannalta, `strftime` tarkoittaa "merkkijonoformaattiaika" ja sillä on juurensa C-ohjelmoinnissa. Pythonin `strftime` tulkitsee muotokoodeja kuten `%Y` vuodelle ja `%m` kuukaudelle, mahdollistaen lähes rajattoman muokattavuuden.

## Katso myös
Sukeltaaksesi syvemmälle Pythonin päivämäärä- ja aikatoimintoihin:
- Pythonin virallinen `datetime` dokumentaatio: https://docs.python.org/3/library/datetime.html
- Niille, jotka ovat kiinnostuneita kattavasta listasta `strftime`-direktiivejä: https://strftime.org/
- Tutustuaksesi kolmannen osapuolen päivämäärä-/aikakirjastoihin:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
