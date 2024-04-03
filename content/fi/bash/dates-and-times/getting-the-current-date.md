---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:00.310952-07:00
description: "Kuinka: Bashissa `date`-komento on ensisijainen ty\xF6kalusi nykyisen\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4n ja ajan saamiseksi. T\xE4ss\xE4 on muutamia esimerkkej\xE4\
  \ sen k\xE4yt\xF6st\xE4: 1. **Hanki\u2026"
lastmod: '2024-03-13T22:44:56.749580-06:00'
model: gpt-4-0125-preview
summary: "Bashissa `date`-komento on ensisijainen ty\xF6kalusi nykyisen p\xE4iv\xE4\
  m\xE4\xE4r\xE4n ja ajan saamiseksi."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

## Kuinka:
Bashissa `date`-komento on ensisijainen työkalusi nykyisen päivämäärän ja ajan saamiseksi. Tässä on muutamia esimerkkejä sen käytöstä:

1. **Hanki nykyinen päivämäärä ja aika oletusmuodossa:**

```bash
date
```

*Esimerkkituloste:*
```
Ke Apr 5 14:22:04 PDT 2023
```

2. **Mukauta tulosteen muotoa:** Voit määrittää tulosteen muodon käyttämällä `+%` muotoiluspesifikaattoreita. Esimerkiksi, näyttääksesi päivämäärän YYYY-MM-DD muodossa:

```bash
date "+%Y-%m-%d"
```

*Esimerkkituloste:*
```
2023-04-05
```

3. **Hanki nykyinen UNIX-aikaleima:** UNIX-aikaleima on sekuntien määrä Unix Epochista (1. tammikuuta 1970) lähtien. Tämä on hyödyllistä skripteille, jotka suorittavat laskelmia aikaerojen perusteella.

```bash
date "+%s"
```

*Esimerkkituloste:*
```
1672877344
```

Tyypillisiä kolmannen osapuolen kirjastoja ei yleensä käytetä tämän perustoiminnon kanssa Bashissa, koska sisäänrakennettu `date`-komento tarjoaa kattavan toiminnallisuuden. Kuitenkin monimutkaisempien päivämäärän ja ajan käsittelyjen yhteydessä ohjelmoijat saattavat käyttää muita ohjelmointikieliä tai työkaluja, jotka tarjoavat kirjastoja päivämäärän aritmetiikkaan ja jäsentämiseen, kuten Pythonin `datetime`-moduuli.
