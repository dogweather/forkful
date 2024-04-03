---
date: 2024-01-27 20:33:33.010958-07:00
description: "Kuinka: Satunnaisluvun generointi Fishissa voi olla suoraviivaista,\
  \ yhdist\xE4m\xE4ll\xE4 j\xE4rjestelm\xE4ty\xF6kaluja ja shellin kykyj\xE4. Alla\
  \ on joitakin esimerkkej\xE4,\u2026"
lastmod: '2024-03-13T22:44:56.988164-06:00'
model: gpt-4-0125-preview
summary: "Satunnaisluvun generointi Fishissa voi olla suoraviivaista, yhdist\xE4m\xE4\
  ll\xE4 j\xE4rjestelm\xE4ty\xF6kaluja ja shellin kykyj\xE4."
title: Satunnaislukujen generointi
weight: 12
---

## Kuinka:
Satunnaisluvun generointi Fishissa voi olla suoraviivaista, yhdistämällä järjestelmätyökaluja ja shellin kykyjä. Alla on joitakin esimerkkejä, jotka osoittavat, miten generoida satunnaislukuja määrätyissä rajoissa.

**Generoi satunnaisluku välillä 0 ja 100:**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**Esimerkkituloste:**
```fish
42
```

**Generoidaan satunnaisluku kahden numeron, sanotaanko 50 ja 150, välillä:**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**Esimerkkituloste:**
```fish
103
```

**Käyttämällä randomia listan sekoittamiseen:**

Saatat haluta myös satunnaista sekoittaa elementtejä listassa. Tässä miten voit tehdä sen:

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**Esimerkkituloste:**
```fish
C
A
E
D
B
```

Huomaa, että tuloste vaihtelee joka kerta, kun suoritat nämä komentorivit satunnaisuuden luonteen vuoksi.

## Syväsukellus
Fish Shellin `random` -funktio tarjoaa helppokäyttöisen rajapinnan pseudo-satunnaislukujen generointiin. Sisäisesti se kietoutuu järjestelmätason satunnaislukujen generointityökalujen ympärille, tarjoten kannettavan tavan tuoda satunnaisuutta skripteihisi. On kuitenkin tärkeää muistaa, että `random`in tarjoama satunnaisuus on riittävä useimmille skriptaus tehtäville, mutta se ei välttämättä täytä korkean tason turvallisuusvaatimuksia sovelluksille, jotka tarvitsevat korkeampaa arvaamattomuuden astetta.

Korkean panoksen turvallisuusyhteyksissä harkitse käyttäväsi omistettuja työkaluja tai ohjelmointikirjastoja, jotka on suunniteltu kryptografisiin tarkoituksiin ja jotka tarjoavat vahvempia satunnaisuustakuita. Siitä huolimatta yleisessä skriptauksessa ja sovelluksissa, joissa korkeimmat turvallisuusstandardit satunnaisuudelle eivät ole vaatimus, Fish Shellin `random` -funktio tarjoaa kätevän ja tehokkaan ratkaisun.
