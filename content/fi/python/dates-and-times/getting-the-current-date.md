---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:37.943062-07:00
description: "Miten: **K\xE4ytt\xE4en standardikirjastoa `datetime`:** `Datetime`-moduuli\
  \ Pythonin standardikirjastossa tarjoaa luokkia p\xE4iv\xE4m\xE4\xE4rien ja aikojen\
  \ k\xE4sittelyyn.\u2026"
lastmod: '2024-04-05T21:53:57.709437-06:00'
model: gpt-4-0125-preview
summary: "**K\xE4ytt\xE4en standardikirjastoa `datetime`:** `Datetime`-moduuli Pythonin\
  \ standardikirjastossa tarjoaa luokkia p\xE4iv\xE4m\xE4\xE4rien ja aikojen k\xE4\
  sittelyyn."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

## Miten:
**Käyttäen standardikirjastoa `datetime`:**

`Datetime`-moduuli Pythonin standardikirjastossa tarjoaa luokkia päivämäärien ja aikojen käsittelyyn. Nykyisen päivämäärän saat `date.today()`-metodilla.

```python
from datetime import date

today = date.today()
print(today)  # Tuloste: VVVV-KK-PP (esim., 2023-04-05)
```

**Ajan muotoilu:**

Jos tarvitset nykyisen päivämäärän eri muodossa, `strftime`-metodi mahdollistaa mukautetun päivämäärän muotoilun:

```python
from datetime import date

today = date.today()
formatted_date = today.strftime('%B %d, %Y')  # Esimerkkimuoto: "huhtikuu 05, 2023"
print(formatted_date)
```

**Käyttäen `pendulum`-kirjastoa joustavuuden lisäämiseksi (suosittu kolmannen osapuolen kirjasto):**

`Pendulum` on kolmannen osapuolen kirjasto, joka tarjoaa intuitiivisemman lähestymistavan päivämäärien ja aikojen käsittelyyn Pythonissa. Se laajentaa standardin datetime-toimintoja ja yksinkertaistaa aikavyöhykkeiden hallintaa, muun muassa.

Ensin varmista, että olet asentanut `pendulum`-kirjaston pip:in kautta:

```shell
pip install pendulum
```

Sitten, nykyisen päivämäärän saamiseksi:

```python
import pendulum

today = pendulum.now().date()
print(today)  # Tuloste: VVVV-KK-PP (esim., 2023-04-05)
```

`Pendulum`-kirjastolla muotoilu on myös suoraviivaista ja samankaltaista kuin `strftime`-lähestymistavassa:

```python
import pendulum

today = pendulum.now()
formatted_date = today.to_formatted_date_string()  # Oletusmuoto: "huhti 5, 2023"
print(formatted_date)
```
