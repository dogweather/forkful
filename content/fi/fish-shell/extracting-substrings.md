---
title:                "Ala-merkkijonojen erottaminen"
html_title:           "Fish Shell: Ala-merkkijonojen erottaminen"
simple_title:         "Ala-merkkijonojen erottaminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Olet varmaan kokenut tilanteen, jossa haluat käsitellä tekstin osaa tai pienempää dataosaa. Tässä Fish Shellin **substring** työkalu tulee avuksi, joka auttaa sinua saamaan haluamasi osan tekstitiedostosta tai merkkijonosta helposti.

## Miten

Käytännössä tämä tarkoittaa, että voit käyttää ``substring`` komentoa hakasulkeiden avulla erottaaksesi haluamasi osan tekstitiedostosta tai merkkijonosta. Katso esimerkki alla:

```Fish Shell
substring "Tämä on esimerkki" 5 -1
```

Tämä palauttaa “on esimerkki” osan tekstitiedostosta. Voit myös käyttää haluamasi alku- ja loppupistettä:

```Fish Shell
substring "Tämä on toinen esimerkki" 0 11
```

Tämä palauttaa “Tämä on toi” osan tekstitiedostosta. Huomaa, että alueen siirtämällä (-1 ensimmäisessä esimerkissä ja 11 toisessa) voit valita haluamasi alueen.

## Syvällisempi sukellus

Substring käyttöön liittyy myös muita ominaisuuksia, kuten pisteotsakierto (point slicing), jossa voit käyttää miinusmerkkejä alueen laskemiseen tekstin lopusta. Voit myös käyttää ``substring`` komentoa yhdessä muiden Fish Shellin työkalujen kanssa, kuten ``string replace``, ``string match`` ja ``string length``.

## Katso myös

- [Fish Shellin viralliset verkkosivut](https://fishshell.com/)
- [Fish Shell opas](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shellin GitHub repository](https://github.com/fish-shell/fish-shell)