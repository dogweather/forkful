---
title:                "Onko hakemisto olemassa? Tarkistaminen"
date:                  2024-01-20T14:56:07.177574-07:00
html_title:           "Gleam: Onko hakemisto olemassa? Tarkistaminen"
simple_title:         "Onko hakemisto olemassa? Tarkistaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Onko hakemisto olemassa?

## Mikä & Miksi?
Tarkistetaan onko hakemisto olemassa. Tämä on pakollista tiedon integriteetin ja virheenhallinnan kannalta.

## How to:
Gleam ei omaa suoraa standardikirjastoa hakemistojen käsittelyyn, joten tämä esimerkki käyttää ulkopuolista `gleam_file`-kirjastoa. Asenna se ennen kokeilua.

```gleam
import gleam/file

pub fn check_dir_exists(path: String) -> Bool {
  file.is_dir(path)
}

fn main() {
  let path = "oma_hakemisto/"
  let exists = check_dir_exists(path)
  
  case exists {
    True -> io.println("Hakemisto löytyy!")
    False -> io.println("Hakemistoa ei ole olemassa.")
  }
}
```

Tulostus riippuen tilanteesta:
```
Hakemisto löytyy!
```
tai
```
Hakemistoa ei ole olemassa.
```

## Syväluotaus
Historiallisesti hakemistojen olemassaolon tarkistus on ollut osa ohjelmointia tiedostojärjestelmäoperaatioiden alusta alkaen. Gleamissa se vaatii usein ulkopuolisen kirjaston, sillä Gleamin vakio-kirjasto keskittyy funktioparadigman ja tyypin turvallisuuden ylläpitämiseen.
Vaihtoehtoisia tapoja tarkistaa hakemistoja on monia ja ne voivat riippua käytössäsi olevista kirjastoista. Voit itse kirjoittaa oman funktion joka hyödyntää esim. Erlangin `filelib`-moduulia käyttäen `gleam/erlang` sidontoja.
Hakemistojen olemassaolon tarkistus on usein IO-operaatio, joka voi epäonnistua tai ottaa aikaa, joten käsittelijän olisi hyvä olla valmis käsittelemään mahdolliset virheet tai viiveet asianmukaisesti.

## Näe Lisäksi
- [gleam_file](https://hex.pm/packages/gleam_file) - Hakemiston tarkistus Gleamissa
- [Erlang filelib](http://erlang.org/doc/man/filelib.html) - Erlangin tiedostokäsittelymoduuli
