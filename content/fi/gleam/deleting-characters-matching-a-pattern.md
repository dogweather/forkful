---
title:    "Gleam: Kaavan mukaisten merkkien poistaminen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa haluat poistaa tietyt merkit kokonaisuudessaan. Ilman Gleamia tämä saattaisi olla työlästä, mutta onneksi Gleamilla on helppo tapa poistaa merkkejä, jotka vastaavat tiettyä kaavaa.

## Kuinka tehdä se

```Gleam
// Luo merkkijono, josta poistat merkit
let syote = "Tämä on esimerkki merkkijonosta!"
// Määritä kaava, joka poistaa "on" kohdassa 5 olevat merkit
let kaava = "on"
// Käytä Gleamin sisäänrakennettua replace-funktiota poistaaksesi merkit
let tulos = replace(syote, kaava, "")
// Tulostaa: "Tämä esimerkki merkkijosta!"
io.println(tulos)
```

## Syventyvä tieto

Tutkimme syvemmin Gleamin replace-funktion toimintaa poistettaessa merkkejä vastaavan kaavan avulla. Tämä funktio hyödyntää syntaksinkäsittelyä ja säännöllisiä lausekkeita tarjotakseen tehokkaan ja käyttäjäystävällisen tavan poistaa merkkejä.

## Katso myös

- Gleamin dokumentaatio: https://gleam.run/
- Esimerkkejä Gleamin replace-funktion käytöstä: https://github.com/gleam-lang/gleam/blob/master/examples/strings.gleam