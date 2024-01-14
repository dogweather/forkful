---
title:                "Gleam: Tiedostojen lukeminen"
simple_title:         "Tiedostojen lukeminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi lukea tekstitiedosto?

Monet ohjelmoijat kohtaavat tilanteita, joissa on tarpeen lukea ja käsitellä tekstitiedostoja. Se voi olla esimerkiksi tietokannan varmuuskopiointi tai tietojen siirto eri järjestelmien välillä. Tämä blogikirjoitus näyttää, miten Gleam-ohjelmointikielellä voit helposti lukea tekstitiedostoja ja käsitellä niiden sisältöä.

## Miten toteuttaa tekstitiedoston lukeminen

Gleamissa on valmiina `File` moduuli, joka tarjoaa toimintoja tiedostojen käsittelyyn. `File.read` -funktio lukee tiedoston sisällön ja palauttaa sen merkkijonona. Katso esimerkki koodi ja tuloste alla:

```Gleam
import File

let result = File.read("tiedosto.txt")
```

Tämä koodi lukee tekstitiedoston nimeltä `tiedosto.txt` ja tallentaa sen sisällön muuttujaan `result`. Voit myös antaa tiedoston polun parametrina `read`-funktiolle, jos tiedosto ei sijaitse nykyisessä kansiossa. Jos tiedostoa ei löydy, `File.read` palauttaa virheilmoituksen.

Jos tiedostossa on useita rivejä, voit pilkkoa sen `String.split` -funktiolla ja käsitellä jokaista riviä erikseen. Esimerkki koodissa alla:

```Gleam
import File

let result = File.read("tiedosto.txt")
let lines = String.split(result, "\n")
```

Tämä koodi lukee tiedoston ja pilkkoo sen rivit, tallentaen ne listana `lines`-muuttujaan. Voit käydä läpi listan `list.map`-funktiolla ja suorittaa haluamasi toiminnon jokaiselle riville.

## Syvempää tietoa tekstitiedoston käsittelystä

Gleam tarjoaa myös muita toimintoja tekstitiedoston käsittelyyn, kuten `read_lines` ja `read_into` -funktiot. `read_lines` lukee ja palauttaa tiedoston rivit listana, kun taas `read_into` lukee tiedoston ja tallentaa sen sisällön muuttujaan, joka on määritelty parametrina. Voit tarkistaa kaikki `File` moduulin tarjoamat toiminnot Gleamin virallisesta dokumentaatiosta.

Jos haluat käsitellä tiedoston sisältöä monimutkaisemmin, voit myös käyttää Gleamin `Regex`-moduulia ja säännöllisiä lausekkeita. Esimerkiksi voit käyttää `Regex.replace_all`-funktiota muuttamaan tietyn merkkijonon kaikki esiintymät haluttuun muotoon.

## Katso myös

- [Gleam virallinen dokumentaatio] (https://gleam.run/) - täältä löydät lisää tietoa Gleamista ja sen tarjoamista toiminnoista
- [Gleam-kurssi] (https://gleam.run/courses/learn-gleam) - interaktiivinen opetusohjelma Gleamin perusteisiin tutustumiseen
- [Gleam-yhteisö] (https://github.com/gleam-lang/gleam/discussions) - liity Gleamin aktiiviseen yhteisöön ja saa apua ja vinkkejä ohjelmoinnissa