---
title:                "Kaavamukaisesti vastaavien merkkien poistaminen"
html_title:           "Rust: Kaavamukaisesti vastaavien merkkien poistaminen"
simple_title:         "Kaavamukaisesti vastaavien merkkien poistaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Poistaessa merkkejä, jotka vastaavat tiettyä kuvioa, ohjelmoijat pystyvät suorittamaan nopeita ja tarkkoja muokkauksia tekstiin. Tämä on hyödyllistä esimerkiksi virheiden korjaamisessa tai tiettyjen tietojen etsimisessä tekstistä.

## Kuinka:
```Rust
let teksti = "Hello world!";
let uusi_teksti = teksti.replace("o", "");
println!("{}", uusi_teksti);
```
> Tulostus: "Hell wrld!"

Tässä esimerkissä käytämme Rustin `replace`-funktiota poistaaksemme kaikki `o`-merkit annetusta tekstistä ja tulostamme uuden tekstin konsoliin.

## Syvempi sukellus:
1. Historiallinen konteksti:
Poistamisen algoritmit ovat olleet käytössä jo 1960-luvulta asti tekstinkäsittelyssä ja ohjelmoinnissa.

2. Vaihtoehdot:
Rustin lisäksi myös monet muut ohjelmointikielet tarjoavat sisäänrakennettuja toimintoja merkkien poistamiseen, kuten Pythonin `replace`-metodi.

3. Toteutus:
Rustin `replace`-funktio käyttää Rustin sisäänrakennettua `Pattern`-tyyppiä vertailemaan merkkejä ja korvaamaan ne tarvittaessa uudella merkillä.

## Katso myös:
- [Rustin dokumentaatio merkkijonojen manipuloinnista](https://doc.rust-lang.org/std/string/)
- [Pythonin `replace`-metodi](https://www.w3schools.com/python/ref_string_replace.asp)