---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:18.852533-07:00
description: "Miten: Fish Shell k\xE4ytt\xE4\xE4 `test`-komentoa tarkistaakseen tiedostotyypit\
  \ ja -ominaisuudet, mukaan lukien onko kohde hakemisto. T\xE4ss\xE4 on perusmalli\u2026"
lastmod: '2024-03-13T22:44:57.009464-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell k\xE4ytt\xE4\xE4 `test`-komentoa tarkistaakseen tiedostotyypit\
  \ ja -ominaisuudet, mukaan lukien onko kohde hakemisto."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

## Miten:
Fish Shell käyttää `test`-komentoa tarkistaakseen tiedostotyypit ja -ominaisuudet, mukaan lukien onko kohde hakemisto. Tässä on perusmalli hakemiston olemassaolon tarkistamiseksi:

```fish
if test -d /polku/hakemistoon
    echo "Hakemisto on olemassa"
else
    echo "Hakemisto ei ole olemassa"
end
```
Esimerkki tuloste:
```
Hakemisto on olemassa
```

Sulavampien tiedosto- ja hakemisto-operaatioiden suorittamiseksi, saattaisi kääntyä ulkoisten työkalujen, kuten `fd`:n puoleen, vaikka sitä käytetään yleisemmin tiedostojen ja hakemistojen löytämiseksi kuin pelkästään olemassaolon tarkistamiseen. Kuitenkin sen yhdistäminen Fish-skriptaukseen voi tuottaa käteviä tuloksia:

```fish
set dir "/polku/hakuun"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "Hakemisto on olemassa"
else
    echo "Hakemisto ei ole olemassa"
end
```

Tämä `fd`-esimerkki etsii hakemistoa määritellyllä syvyydellä, ja `grep` tarkistaa vastaavuuden, tehden siitä monipuolisen hienovaraisiin tarkistuksiin. Kuitenkin, suoraan olemassaolon tarkistamisen tarkoitukseen, Fishin sisäänrakennetun `test`:n käyttö on sekä tehokasta että suoraviivaista.
