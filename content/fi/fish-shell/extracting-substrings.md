---
title:    "Fish Shell: Merkkijonojen erottaminen"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi: Miksi haluaisit poimia osastringejä?

Osastringien poimiminen on tärkeä osa Fish Shell -ohjelmoinnissa, koska se mahdollistaa tietyn osan merkkijonosta erottamisen ja käyttämisen. Tämä voi olla hyödyllistä esimerkiksi tiedostojen tai muuttujien nimien käsittelyssä.

## Kuinka: Koodiesimerkkejä ja tulosteita "```Fish Shell ... ```" -koodilohkoissa.

#### Koodiesimerkki 1:
```
set name "Fish Shell"
echo $name[5..-2]
```
#### Tuloste:
```
Shel
```

Tässä esimerkissä "Fish Shell" -merkkijonosta poimitaan osastringi "Shel" käyttämällä sulkumerkkejä ja indeksiä.

#### Koodiesimerkki 2:
```
set filename "blog_post.md"
echo $filename[-5..]
```
#### Tuloste:
```
post.md
```

Tässä esimerkissä tiedostonimessä poimitaan osastringi "post.md" käyttämällä negatiivista indeksiä.

## Syvemmälle: Tietoa osastringien poimimisesta.

Fish Shell tarjoaa erilaisia tapoja poimia osastringejä merkkijonoista. Alla on muutamia esimerkkejä eri tavoista ja niiden selitykset.

#### Sulkumerkkejä ja indeksejä käyttäminen:
Sulkumerkkejä [] voidaan käyttää määrittämään tietty indeksi tai indeksien alue, josta halutaan poimia osastringi. Indeksit voivat olla myös negatiivisia, mikä tarkoittaa, että laskeminen tapahtuu merkkijonon lopusta.

#### Otsikon poimiminen merkkijonon alusta:
Jos haluat poimia osastringin merkkijonon alusta, voit käyttää seuraavaa syntaksia: ```$merkkijono[indeksi..]```

#### Otsikon poimiminen merkkijonon lopusta:
Jos haluat poimia osastringin merkkijonon lopusta, voit käyttää seuraavaa syntaksia: ```$merkkijono[..indeksi]```

Syvemmät tiedot ja esimerkit löydät Fish Shellin virallisilta verkkosivuilta dokumentaatiosta.

## Katso myös:
- [Fish Shell -dokumentaatio](https://fishshell.com/docs/current/)
- [Substring Extraction in Fish Shell](https://dzone.com/articles/substring-extraction-in-fish-shell)