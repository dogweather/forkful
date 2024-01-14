---
title:    "Gleam: Komentoriviparametrien lukeminen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Käyttöliittymätehosteet ovat erittäin tärkeitä kaikenlaisessa ohjelmoinnissa, ja konsoli on yksi yleisimmistä tapoista käyttää niitä. Gleam-ohjelmointikielessä on helppo lukea komentorivin argumentteja. Tässä blogikirjoituksessa jaamme yksinkertaisia ​​tapoja toteuttaa tämä ominaisuus Gleamissa ja miten se voi helpottaa kehitystyötäsi.

## Miten

Komentorivin argumenttien lukemiseksi Gleamissa voit käyttää vakiofunktiota `os.args()`, joka palauttaa sanakirjan argumenteista. Voit käyttää tätä funktiota helposti koodissa "```Gleam
args = os.args()
"```". Voit sitten käydä läpi jokaisen argumentin `List.map()`-funktion avulla ja suorittaa haluamasi toiminnot.

```Gleam
List.map(args, \arg -> IO.println(arg))
```

Yllä olevassa koodissa jokainen argumentti tulostetaan konsoliin omalle rivilleen. Voit myös käyttää `List.filter()`-funktiota etsiäksesi tiettyä argumenttia ja tehdä sille tiettyjä toimenpiteitä.

## Syvempi sukellus

Funktio `os.args()` palauttaa sanakirjan, joka sisältää argumenttien lisäksi myös muita tietoja, kuten ympäristömuuttujat ja nykyisen työskentelykansion. Voit käyttää tätä tietoa hyödyksesi, kun kehität monimutkaisempia ohjelmia.

Lisäksi voit käyttää `os.args()`-funktiota myös Gleam-projektisi testaamiseen. Voit lähettää erilaisia argumentteja suorittaaksesi erilaisia koodipolkuja ja varmistaa, että ohjelmasi toimii oikein.

## Katso myös

- [Gleam-kielen viralliset verkkosivut](https://gleam.run/)
- [Gleam-kielen dokumentaatio](https://gleam.run/book/)
- [Gleam-kielen GitHub-sivu](https://github.com/gleam-lang/gleam)