---
title:    "Gleam: Tiedostotiedoston lukeminen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi lukea tekstitiedostoa Gleam-ohjelmoinnilla?

Jos olet Gleam-ohjelmoija ja haluat käsitellä andataa tekstimuodossa, luettaessa tekstitiedostoa voi olla erittäin hyödyllistä. Tämä blogikirjoitus opastaa sinua lukemaan tekstitiedostoja Gleam-ohjelmointikielen avulla ja antaa sinulle vinkkejä siitä, miten tämän teet.

## Miten lukea tekstitiedosto Gleam-ohjelmoinnilla?

Lukeminen tekstiideltiedostoja Gleamilla on helppoa! Käytä vain ```File``` -moduulia ja sen ```open``` -funktiota avataksesi tiedoston ja ```read_line``` -funktiota lukemaan sen sisällön rivi kerrallaan. Tässä on esimerkki:

```Gleam
import File

let file = File.open("tiedosto.txt")
for line in File.read_line(file) {
  io.println(line)
}

```

Tämä koodi avaa tiedoston nimeltä "tiedosto.txt" ja tulostaa sen sisällön konsoliin.

## Syväsukellus: miten tiedostot luetaan Gleamilla?

Tiedostojen lukeminen tapahtuu useimmiten kahdessa vaiheessa: tiedoston avaaminen ja sen sisällön lukeminen. Avaa tiedosto käyttämällä ```File.open``` -funktiota ja anna sille tiedoston nimi kirjoitettuna lainausmerkeissä. Sitten voit lukea tiedoston sisällön käyttämällä ```read_line``` -funktiota, joka lukee tiedoston rivit yksi kerrallaan kunnes pääsee tiedoston loppuun. Voit myös käyttää muita funktioita, kuten ```read_all``` ja ```read_binary``` riippuen siitä, miten haluat käsitellä tiedoston sisältöä.

## Katso myös

- [Gleam-kielen virallinen verkkosivusto](https://gleam.run/)
- [Gleam-kielen dokumentaatio](https://gleam.run/documentation/)
- [Gleam-kielen Github-repositorio](https://github.com/gleam-lang/gleam)