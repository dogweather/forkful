---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Bash: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Luodaan väliaikaisia tiedostoja Rust-ohjelmointikielellä

## Mikä & Miksi?

Väliaikaisten tiedostojen luonti tarkoittaa tilapäisten tiedostojen luomista ja käyttöä ohjelman suorituksen ajaksi. Tämä on hyödyllistä esimerkiksi suurien tietojen väliaikaiseen tallennukseen tai tiedostojen väliaikaiseen jakamiseen eri prosessien välillä.

## Kuinka tehdä:

```Rust
use std::fs::File;
use std::io::Write;
use tempfile::tempfile;

let mut tiedosto = tempfile().unwrap();

write!(tiedosto, "Hei maailma!").unwrap();
```
Kun tämä koodi suoritetaan, se luo väliaikaisen tiedoston ja kirjoittaa siihen merkkijonon "Hei maailma!".

## Syvempi sukellus

Historiallisesti väliaikaisia tiedostoja on käytetty monissa ohjelmissa ja järjestelmissä resurssien optimointiin. Olemassa on myös muita vaihtoehtoja väliaikaisten tiedostojen käytölle, kuten yhteiskäytössä olevat muistialueet (shared memory areas) tai jopa tietokannat.

Rustin väliaikaisten tiedostojen luonnissa käytettävä `tempfile::tempfile()` on abstraktio, joka huolehtii väliaikaisten tiedostojen tiedostoteiden luonnista ja niiden poistamisesta ohjelman suorituksen päättyessä. Käytännössä `tempfile()` luo tiedoston standardin väliaikaisten tiedostojen hakemiston alla ja palauttaa `File`-käsittelyn, jota voidaan lukea ja kirjoittaa.

Se, missä väliaikaisia tiedostoja säilytetään, riippuu käyttöjärjestelmästä. Linuxissa ja Unix-järjestelmissä yleensä `/tmp` tai `/var/tmp` hakemistoja käytetään tähän tarkoitukseen.

## Katso myös

- [Rust Doc: `tempfile`](https://docs.rs/tempfile/3.1.0/tempfile/): Täydellinen dokumentointi `tempfile::tempfile()` funktion käytöstä.
- [Rust By Example](https://doc.rust-lang.org/rust-by-example/): Kattava opas Rust-ohjelmointiin, mukaan lukien tiedostojen käsittely.