---
title:                "Gleam: Komentoriviparametrien lukeminen"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi?

Oletko koskaan huomannut, että kun suoritat ohjelmasi terminaalissa, voit lisätä siihen lisätoimintoja kirjoittamalla komentoriviparametreja? Gleam-ohjelmointikieli tarjoaa mahdollisuuden lukea näitä komentoriviparametreja ja hyödyntää niitä ohjelmasi toiminnassa. Tässä blogikirjoituksessa kerromme, miksi haluat ehdottomasti tutustua tähän ominaisuuteen.

## Kuinka?

```Gleam
module Hakija {

import gleam/io
import gleam/strings

pub fn main(arguments) {
  strings.split(arguments, " ") 
  |> io.printf("Komentoriviparametreja: %s") 
}
``` 

Tämä yksinkertainen koodiesimerkki näyttää, kuinka voit lukea komentoriviparametreja Gleam-ohjelmassa. Käyttämällä split-toimintoa voit jakaa parametrit eri osiksi ja hyödyntää niitä ohjelman suorituksessa. Tässä esimerkissä ohjelma tulostaa yksinkertaisesti kaikki parametrit.

```
$ gleam run hakija.gleam ensimmäinen toinen kolmas
Komentoriviparametreja: ["ensimmäinen", "toinen", "kolmas"]
```

## Syvempi sukellus

Komentoriviparametrien lukeminen on hyödyllistä, kun haluat esimerkiksi ajaa saman ohjelman eri parametreilla tai muokata ohjelman toimintaa parametrien avulla. Voit myös tarkistaa, mitä parametreja käyttäjä antaa ohjelmallesi ja toimia sen mukaan.

Voit lukea lisää Gleamin komentoriviparametreista virallisesta dokumentaatiostamme: [https://gleam.run/manual/command-line-arguments.html](https://gleam.run/manual/command-line-arguments.html)

## Katso myös

- [Gleamin virallinen dokumentaatio](https://gleam.run)
- [Gleam-yhteisön keskustelufoorumi](https://forum.gleam.run)
- [Gleam-pikaopas](https://gleam.run/getting-started.html)