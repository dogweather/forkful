---
title:    "Gleam: Väliaikaistiedoston luominen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Miksi

Luodessaan ohjelmia, yksi tärkeä asia on työskennellä tehokkaasti ja turvallisesti. Yksi tapa saavuttaa tämä on käyttämällä väliaikaisia tiedostoja, joita ei ole tarpeen tallentaa pysyvästi.

## Miten

Gleam tarjoaa kätevän tavan luoda väliaikaisia tiedostoja ohjelmassa. Se tehdään käyttämällä `temp_file` funktiota, joka hyväksyy yhden argumentin eli merkkijonon, joka toimii tiedostonimen pohjana.

```Gleam
let temporary_file = temp_file("example");
```

Tämän jälkeen, Gleam luo tiedoston `example` nimen ja lisää siihen satunnaisen numeron varmistaen, että tiedostonimi on uniikki. Tämän tiedoston voi sitten käyttää kuten mitä tahansa muuta tiedostoa ohjelmassa.

```Gleam
// Kirjoitetaan tiedostoon
File.write(temporary_file, "Tässä on tekstiä");

// Luetaan tiedostosta
let content = File.read(temporary_file);
```

Tärkeää on huomata, että tiedosto poistetaan automaattisesti, kun ohjelma suoritus päättyy. Tämä varmistaa, että väliaikaiset tiedostot eivät jää turhaan tallennettuna ja vie ylimääräistä tilaa.

## Syvempi sukellus

`temp_file` funktio hyödyntää alustan omia väliaikaisia tiedostoja. Gleam vain tarjoaa kätevän API:n tälle toiminnallisuudelle, joten kehittäjän ei tarvitse huolehtia tiedostojen luomisesta ja nimeämisestä.

Väliaikaisten tiedostojen käyttö on hyvä tapa välttää turhia pysyviä tiedostoja ohjelman suorituksen aikana. Tämä on erityisen hyödyllistä esimerkiksi web-sovelluksissa, joissa on tarve luoda väliaikaisia tiedostoja käyttäjien lataamille tiedostoille.

## Katso myös

- [Gleamin virallinen dokumentaatio](https://gleam.run/documentation/guide/temporary_files)
- [Gleamin Github-repositorio](https://github.com/gleam-lang/gleam)