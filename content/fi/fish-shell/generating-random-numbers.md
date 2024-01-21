---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:49:09.995182-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä ja miksi? Satunnaisluvut ovat arvaamattomia numeroita, jotka tietokone luo. Ohjelmoijat käyttävät niitä esimerkiksi testaamisessa, peleissä ja turvallisuudessa - milloin tahansa, kun tarvitaan odottamatonta elementtiä.

## How to:
Miten tehdään: 

```Fish Shell
# Luodaan satunnaisluku väliltä 1-100
set -l random_number (random 1 100)
echo $random_number 
```

Esimerkkituloste voisi olla:

```Fish Shell
42
```

## Deep Dive
Syväsukellus: Satunnaislukujen generointi on ollut keskeinen osa ohjelmointia alusta asti. Ennen `random`-komentoa Fishissä, vanhemmissa kuorissa saattoi joutua käyttämään kolmannen osapuolen työkaluja tai monimutkaisia temppuja luvun arpomiseen. Fish Shell tekee prosessista yksinkertaisen sisäänrakennetulla `random`-komennolla.

Fishin `random` on deterministinen, mikä tarkoittaa, että se tuottaa saman sarjan numeroita samalla siemenellä (seed). Tämä on hyödyllistä testauksessa. Vaihtoehtoisen lähestymistavan tarjoaa esimerkiksi `/dev/urandom` Unix-pohjaisissa järjestelmissä, joka tuottaa kryptografisesti turvallisia satunnaislukuja.

## See Also
Katso myös:
- Fish Shellin dokumentaatio: https://fishshell.com/docs/current/cmds/random.html
- Unix `/dev/urandom`: https://linux.die.net/man/4/urandom
- Satunnaisuuden matemaattinen perusta: https://en.wikipedia.org/wiki/Randomness