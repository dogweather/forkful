---
title:    "Gleam: Tarkista, onko hakemisto olemassa"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Miksi tarkistaa, onko hakemisto olemassa? 

Monissa ohjelmointiprojekteissa on tärkeää tarkistaa, onko tietty hakemisto olemassa ennen sen käyttämistä. Tämä voi olla esimerkiksi tietokantaan tallennettujen tiedostojen paikan varmistamiseksi tai käyttäjän tekemien valintojen käsittelyn helpottamiseksi. Gleam-kielen avulla tämä on helppoa ja näytämme sinulle miten!

## Miten tehdä tämä Gleamilla

```Gleam
let directory = "./data"

let exists = std.fs.exists(directory)

if exists {
  // Hakemisto on jo olemassa, voit jatkaa sen käyttöä
  // esimerkiksi tiedostojen tallentamiseen
} else {
  // Hakemistoa ei löytynyt, voit luoda sen tarvittaessa
  std.fs.create_dir(directory)
}
```

## Syöte ja tulostus

Oletetaan, että käytettävissä on hakemisto nimeltä "data". Kun suoritamme koodin, saamme tämän tulosteen:

```
// Syöte
"./data"

// Tulostus
Hakemisto löytyi, voit siirtää tiedostoja sinne.
```

Jos hakemistoa ei löydy, saat tämän tulosteen:

```
// Syöte
"./data"

// Tulostus
Hakemistoa ei löydy, se luodaan automaattisesti.
```

## Syvällinen sukellus

Gleam-kielen `std.fs` -kirjasto tarjoaa kätevät toiminnot tiedostojen ja hakemistojen hallintaan. `exists`-funktio palauttaa `true` tai `false` sen mukaan, onko annetulla polulla oleva tiedosto tai hakemisto olemassa. Tämä on hyödyllinen tarkistus ennen esimerkiksi tiedostojen avaamista tai luomista.

## Katso myös

- Gleamin virallinen dokumentaatio: https://gleam.run/
- Virallinen kielikirjasto, mukaan lukien `std.fs`: https://github.com/gleam-lang/gleam_stdlib
- Ohjeita Gleam-kielen aloittamiseen: https://gleam-lang.org/guides/getting-started/