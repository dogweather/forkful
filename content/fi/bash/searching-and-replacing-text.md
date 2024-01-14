---
title:    "Bash: Tekstin etsiminen ja korvaaminen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Tekstin etsiminen ja korvaaminen on tärkeä osa Bash-ohjelmointia, sillä se säästää aikaa ja vaivaa manuaalisesti muuttaessa tekstiä. Tekstin etsiminen ja korvaaminen on myös hyödyllinen taito käyttääkseen Bash-ohjelmointia laajemmassa kontekstissa.

## Kuinka tehdä

Bash tarjoaa useita eri työkaluja tekstin etsimiseen ja korvaamiseen. Yksinkertaisimpia ovat `grep` ja `sed`. Alla on esimerkki komennosta, joka etsii "tekstiä" tiedostosta "tiedosto.txt" ja korvaa sen "uudella tekstillä".

```Bash
grep -rl "tekstiä" tiedosto.txt | xargs sed -i 's/tekstiä/uusi teksti/g'
```

Tässä ensin käytetään `grep`:ia etsimään kaikki rivit, jotka sisältävät "tekstiä" tiedostosta "tiedosto.txt". Tulokset syötetään sitten `xargs`:lle, joka puolestaan syöttää ne `sed`:lle. `sed` sitten korvaa kaikki "tekstiä" esiintymät "uudella tekstillä" käyttäen `-i`-valitsinta pysyvästi muuttaen alkuperäistä tiedostoa.

## Syvällinen sukellus

Tekstin etsiminen ja korvaaminen voi olla monimutkaista, kun joudutaan käsittelemään monimutkaisia säännöllisiä lausekkeita ja erityisiä muuttujia. `sed` tarjoaa kuitenkin monia vaihtoehtoisia ja edistyneitä käyttötapoja, kuten rajaavien merkkien käyttö, rivinumeroiden ja hakulausekkeiden käyttö sekä monen rivin käsittely. On tärkeää ottaa aikaa tutustua `sed`:n dokumentaatioon ja kokeilla erilaisia komentoja ja vaihtoehtoja löytääkseen tehokkaimman tavan ratkaista tietty ongelma.

## Katso myös

- Bashin virallinen dokumentaatio tekstin etsimisestä ja korvaamisesta: https://www.gnu.org/software/bash/manual/html_node/Searching-and-Replacing.html
- Käytännön esimerkkejä tekstin etsimisestä ja korvaamisesta: https://linuxize.com/post/how-to-find-and-replace-text-string-in-files-in-linux/