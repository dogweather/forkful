---
title:                "Bash: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi lukisi tekstikirjoja? Eikö ole parempi tapa viettää aikaa? Kyllä ja ei. Aika ajoin, joskus päivittäin, työ tarvitsee jokainen tyyppi antamaan se alkuperäisen yrittämiseksesi ajatella aina vähemmän ta ottaa kykyjä. Piirtämällä yksityiskohtaiset lähtökohdat nähteisiin asiakkaan pitäisi lisä loogisesti neuvoja muutoksista. Lukeminen tekstitiedostoja voi auttaa kehittämään kykyjä ja taitoja.

## Miten

Lukeminen tekstitiedostoja bash-ohjelmointikielessä on helppoa. Ensiksi, avaa Terminal-ikkuna ja siirry hakemisto, jossa haluat lukea tiedoston. Voit käyttää "cd" komentoa navigoidaksesi haluamaasi hakemistoon.

Sitten, kirjoita seuraava komento:

```Bash
cat tiedostonimi.txt 
```

Tämä komento tulostaa tiedoston sisällön suoraan Terminaalissa.Haluat ehkä muuntaa tai käsitellä tiedoston sisältöä. Voit tehdä sen käyttämällä erilaisia komentoja, kuten "grep", "sed" tai "awk". Esimerkiksi voit etsiä tiettyjä rivejä tiedostosta käyttämällä "grep" komentoa:

```Bash
grep "hakusana" tiedostonimi.txt 
```

Tämä tulostaa kaikki rivit, jotka sisältävät haetun sanan.

## Syventävä tarkastelu

Lukeminen tekstitiedostoja on tärkeä taito jokaiselle bash-ohjelmoijalle. Se auttaa sinua ymmärtämään tiedostorakennetta ja muokkaamaan tiedoston sisältöä. Myös käyttäessäsi muita ohjelmointikieliä, tiedoston lukemisen taito on hyödyllinen.

Voit myös käyttää bash-skriptejä automatisoimaan tiedoston lukemista ja muokkaamista. Tämä säästää aikaa ja vaivaa, kun joudut käsittelemään suuria määriä tiedostoja.

## Katso myös

- [Bash-pikakurssi](https://linux.fi/wiki/Bash-pikakurssi)
- [Bash-ohjelmointikielen perusteet](https://devdocs.io/bash/)
- [Bash-skriptaus opas](https://www.linux.com/learn/tutorials/306404-write-a-real-world-bash-script-basics)