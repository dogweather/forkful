---
title:                "Bash: Alimerkkijonojen erottelu"
simple_title:         "Alimerkkijonojen erottelu"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien erottaminen on tärkeä osa Bash-ohjelmointia, sillä se mahdollistaa tiettyjen merkkijonojen erottamisen isommista kokonaisuuksista. Tämä voi olla erityisen hyödyllistä, kun haluat muokata tiettyä osaa merkkijonosta tai haluat käyttää vain pienen osan tietystä merkkijonosta.

## Miten tehdä

Bash-ohjelmointikielessä voit käyttää komentoa `cut` erottaaksesi haluamasi osat merkkijonosta. Esimerkiksi jos haluat erottaa ensimmäisen sanan merkkijonosta, voit käyttää seuraavaa komentoa:

```Bash
cut -d " " -f 1 <<< "Tämä on esimerkki merkkijonosta"
```

Tämä palauttaa vain ensimmäisen sanan, eli "Tämä", käyttäen välilyöntiä erotinmerkkinä (`-d`). Voit myös muuttaa delimiterin esimerkiksi pilkuksi vaihtamalla sen `-d` -valintaan.

Voit myös erottaa osan merkkijonosta käyttäen `substring` -toimintoa Bashissa. Esimerkiksi:

```Bash
merkkijono="Tämä on esimerkki merkkijonosta"
echo ${merkkijono:0:4}
```

Tämä palauttaa vain ensimmäisen neljännen merkin, eli "Tämä", merkkijonosta.

## Syvällisempi sukellus

Bash tarjoaa monia mahdollisuuksia erilaisten substringien erottamiseen. Voit esimerkiksi käyttää `grep` -komentoa etsiäksesi tietyn merkkijonon ja erottaa sen käyttämällä `cut` tai `substring` -toimintoa.

Voit myös käyttää `awk` -komentoa monimutkaisempien substringien erottamiseen. `awk` käyttää erityistä syntaksia, jolla voit määrittää tietyn osan merkkijonosta.

## Katso myös

- [Bashin virallinen dokumentaatio](https://www.gnu.org/software/bash/manual/)
- [Bashin AWK syntaksi -opas](https://www.pankajtanwar.in/basics-of-awk-command-in-linux/)
- [Cut ja Substring -komennon käyttö Bashissa](https://linuxhint.com/cut-substring-bash/)