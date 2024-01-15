---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Gleam: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Miksi

Miksi haluaisit tarkistaa, onko hakemisto olemassa? Yksinkertaisesti sanottuna, haluat ehkä varmistaa, että kyseinen hakemisto on olemassa ennen kuin yrität tehdä siihen jotain, kuten tallentaa tiedostoja tai hakea tietoja sieltä.

## Miten

```Gleam
let result = File.Directory.exists("polku/hakemistoon")

match result {
  Ok(val) -> // Hakemisto löytyi, tee tarvittavat toimenpiteet
  Err(err) -> // Hakemistoa ei löytynyt, tee tarvittavat toimenpiteet
}
```

Tässä esimerkissä käytetään `File.Directory.exists` -funktiota tarkistaaksesi, onko annettu hakemisto olemassa. Jos hakemisto löytyy, `Ok` -haaraa käytetään suorittamaan kyseisiä toimenpiteitä. Jos hakemistoa ei löydy, `Err` -haaraa käytetään käsittelemään virheellinen tilanne.

```Gleam
let hakemisto = File.Directory.create("polku/uusi_hakemisto")

match hakemisto {
  Ok(val) -> // Hakemisto luotiin onnistuneesti
  Err(err) ->
    match err {
      DirectoryAlreadyExists -> // Hakemisto on jo olemassa, tee tarvittavat toimenpiteet
      _ -> // Jokin muu virhe, käsittele se
    }
}
```
Tässä toisessa esimerkissä käytämme `File.Directory.create` -funktiota luodaksemme uuden hakemiston annettuun polkuun. Jos hakemisto luodaan onnistuneesti, `Ok` -haaraa käytetään. Muussa tapauksessa tarkistamme `Err` -haaran avulla, onko kyseessä `DirectoryAlreadyExists` -virhe, eli hakemisto on jo olemassa, vai jokin muu virhe.

## Syvemmälle

Gleamin `File.Directory` -moduuli tarjoaa muutamia hyödyllisiä käytännön toimintoja tarkistaaksesi ja hallitaksesi hakemistoja. Esimerkiksi `File.Directory.list` -funktio antaa sinulle listan hakemistoon sisältyvistä tiedostoista ja alihakemistoista. `File.Directory.delete` -funktio puolestaan poistaa annetun hakemiston, jos se on olemassa.

Ja jos haluat päästä vielä syvemmälle Gleamin tiedostojärjestelmän maailmaan, voit tarkistaa [viralliset dokumentaatiot](https://gleam.run/modules/file.directory.html) tai tutustua [tiedostojärjestelmän käsitteisiin](https://gleam.run/concepts/file-system.html) Gleamin oppaassa.

# Katso myös

- [Gleamin `File.Directory` -moduulin dokumentaatio](https://gleam.run/modules/file.directory.html)
- [Tiedostojärjestelmän käsitteet Gleamin oppaassa](https://gleam.run/concepts/file-system.html)