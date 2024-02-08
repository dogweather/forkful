---
title:                "Tarkistetaan, onko hakemisto olemassa"
aliases:
- fi/fish-shell/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:18.852533-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
Hakemiston olemassaolon tarkistaminen Fish Shellissä mahdollistaa skriptien tehdä päätöksiä perustuen hakemistorakenteiden läsnäoloon tai puuttumiseen, mahdollistaen tehtäviä kuten ehdolliset tiedosto-operaatiot, lokitiedot tai ympäristön asetukset. Tämä tekniikka on olennainen osa kestävien skriptien kirjoittamisessa, jotka vuorovaikuttavat tiedostojärjestelmän kanssa ennustettavalla tavalla.

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
