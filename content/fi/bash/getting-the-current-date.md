---
title:                "Bash: Päivämäärän hankkiminen tietokoneohjelmoinnissa"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi
Bash-ohjelmointi on monipuolista ja sen avulla voi tehdä monia käteviä asioita, kuten esimerkiksi hakea tietoja internetistä tai käsitellä tekstitiedostoja. Yksi hyödyllinen toiminto, jota Bashilla voi tehdä, on nykyisen päivämäärän hakeminen. Tässä blogikirjoituksessa käymme läpi, kuinka tämä onnistuu.

## Näin teet sen
Ensiksi, avaa Bash terminaali ja kirjoita seuraava komento:

```Bash
date
```

Tämä komento tulostaa nykyisen päivämäärän ja kellonajan seuraavassa muodossa:

```Bash
ma marrask.  4 20:35:30 EET 2019
```

Voit myös valita minkä muodossa haluat tulostetun päivämäärän lisäämällä erilaisia argumentteja komentoon. Esimerkiksi, jos haluat tulostaa pelkän päivämäärän numeroina, voit käyttää seuraava komento:

```Bash
date +%m%d%Y
```

Se tuottaa tuloksen seuraavassa muodossa:

```Bash
11042019
```

Voit myös muokata päivämäärän ja kellonajan asetuksia komennolla. Esimerkiksi, jos haluat näyttää vain vuoden, voit käyttää seuraavaa komentoa:

```Bash
date +%Y
```

Tämä tulostaa seuraavasti:

```Bash
2019
```

## Syvemmälle asiaan
Voit lisätä myös muita komentoja päivämääräkomennon kanssa, jotta saat tarkempia tietoja. Esimerkiksi, jos haluat tarkistaa, mikä viikonpäivä on, voit lisätä komennon loppuun " +%A" ja se tulostaa viikonpäivän koko nimellä.

```Bash
date +%A
```

Tämä tulostaa esimerkiksi seuraavasti:

```Bash
maanantai
```

Lisäksi, voit lisätä komennon loppuun myös muita muotoiluja, kuten päivän lyhentyneet nimet (%a), kuukauden numero (%m) tai kellonajan erilaiset formaatit. Käytössä on paljon erilaisia vaihtoehtoja, joten kannattaa kokeilla ja löytää itselleen sopiva tapa tulostaa päivämäärät.

## Katso myös
- [Bash Commands Cheat Sheet](https://devhints.io/bash)
- [Bash Scripting Tutorial for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Bash Date command documentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)