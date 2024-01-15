---
title:                "Yaml: Työskentely ohjelmointikielen kanssa"
html_title:           "Gleam: Yaml: Työskentely ohjelmointikielen kanssa"
simple_title:         "Yaml: Työskentely ohjelmointikielen kanssa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

Yksi tärkeimmistä tekijöistä, jotka tekevät YAML:stä tärkeän, on sen yksinkertaisuus ja helppokäyttöisyys. YAML-tiedostot ovat loistava tapa tallentaa ja jakaa rakenteellisia tietoja.

## How To

Käyttääksesi YAML:ää kätevästi Gleamissa, tarvitset `gleam-serial` -kirjaston, jota voit asentaa seuraavalla komennolla:

```gleam
gleam install gleam-serial
```

Seuraavaksi sinun tulee määrittää kuinka haluat käyttää YAML:ää tietojen tallentamiseen. Voit esimerkiksi luoda rakenteen, joka sisältää merkkijonon, kokonaisluvun ja listan, ja muuntaa sen YAML-muotoon seuraavasti:

```gleam
import gleam/serial

pub struct Henkilo {
  nimi: String,
  ika: Int,
  harrastukset: List(String),
}

let mina: Henkilo = Henkilo (
  nimi: "Matti",
  ika: 25,
  harrastukset: ["luistelu", "lukeminen", "sisustaminen"]
)

let yaml = serial.encode(mina)

gleam/core/format.println(yaml)
```

Tulostus:

```
nimi: Matti
ika: 25
harrastukset:
  - luistelu
  - lukeminen
  - sisustaminen
```

Voit myös luoda Gleam-tyyppejä suoraan YAML-tiedostosta käyttämällä `decode` -funktiota:

```gleam
let tulokset = serial.decode[Henkilo](yaml)

gleam/core/format.println(tulokset)

```

Tulostus:

```
Result.Ok(
  Henkilo(
    nimi: "Matti",
    ika: 25,
    harrastukset: ["luistelu", "lukeminen", "sisustaminen"]
  )
)
```

## Deep Dive

YAML-tiedosto koostuu avaimista ja arvoista, jotka on jaoteltu sisennyksillä. Avaimet ja arvot ovat erotettu kaksoispisteellä (`:`), ja arvot voivat olla joko yksittäinen arvo (esim. merkkijono tai luku) tai lista.

Kun käytät `decode` -funktiota, määrittele Gleam-tyyppi sen mukaan, millaisen YAML-tiedoston haluat luoda. Jos haluat määritellä hiontalistan, voit käyttää `List(Henkilo)`.

Gleam-serial tarjoaa myös mahdollisuuden käsitellä virheitä `decode` -funktion kanssa käyttämällä `Result` -tyyppiä. Tämä on erityisen kätevää, jos haluat varmistaa, että YAML-tiedosto on oikein muotoiltu.

## Katso myös

- [YAML-syntaksiopas](https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/)
- [Gleam-serialin dokumentaatio](https://github.com/gleam-lang/serial)