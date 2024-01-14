---
title:                "Gleam: Kansion olemassaolon tarkistaminen"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa, onko kansio olemassa?

Kansioita tarvitaan usein ohjelmoinnissa tiedostojen järjestämiseen ja hallintaan. On tärkeää tarkistaa, onko tietty kansio olemassa ennen kuin yrität käsitellä sitä, jotta vältytään mahdollisilta virheiltä ja ohjelman kaatumiselta.

## Kuinka tarkistaa, onko kansio olemassa

Tässä esimerkissä käytämme Gleam-ohjelmointikieltä, joka soveltuu hyvin kansiohakemistojen käsittelyyn.

```Gleam
import gleam/io

// Tarkistetaan, onko kansio "hakemisto" olemassa ja vastataan sen perusteella
match io.dir_exists("hakemisto") {
  True -> io.println("Kansio on olemassa")
  False -> io.println("Kansiota ei ole olemassa")
}
```

Jos kansio "hakemisto" on olemassa, ohjelma tulostaa "Kansio on olemassa". Muussa tapauksessa tulostetaan "Kansiota ei ole olemassa".

## Syventyvä tarkastelu

Kansiohakemistojen käsittelyyn on myös muita tapoja Gleam-ohjelmointikielessä. Joihinkin menetelmiin sisältyy virhehallinta, joka tarjoaa mahdollisuuden reagoida ohjelman suoritusaikana tapahtuneisiin virheisiin. Voit myös tarkistaa, onko tietyssä hakemistopolussa tiedostoja tai alihakemistoja käyttämällä `io.file_exists()` tai `io.dir_children()` -funktioita.

See Also:

- [Gleam käyttöohjeet](https://gleam.run/)
- [Pythonin hakemistonhallinta](https://docs.python.org/3/library/os.html#os.path.isdir)
- [Rust kansiohakemiston käsittely](https://doc.rust-lang.org/std/fs/fn.metadata.html)