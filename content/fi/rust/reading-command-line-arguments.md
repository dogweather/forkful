---
title:                "Lukemalla komentoriviparametrit"
html_title:           "Rust: Lukemalla komentoriviparametrit"
simple_title:         "Lukemalla komentoriviparametrit"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Lue komentorivi argumentteja on tekniikka, joka mahdollistaa käyttäjien syöttämien tietojen lukemisen ohjelmaan komentoriviltä. Tämä antaa ohjelmoijille mahdollisuuden muokata ohjelmiaan käyttäjän tarpeiden mukaan.

## Kuinka tehdä:
Esimerkiksi, voit lukea komentorivi argumentteja Rust-ohjelmassa käyttämällä vakioa ```args```. Tämä vakio sisältää vektorin komentorivi argumenteista ja sen avulla voit käsitellä saatuja tietoja.
```
use std::env;
fn main() {
    let args: Vec<String> = env::args().collect();

    println!("Ensimmäinen argumentti on: {}", args[1]);
}
```
Kun käyttäjä antaa komentorivi argumentin, kuten ```rust ohjelma.rs argumentti```, ohjelma tulostaa: "Ensimmäinen argumentti on: argumentti".

## Syvä sukellus:
Komentorivi argumenttien lukeminen on ollut tärkeä osa ohjemointia jo pitkään. Ennen kuin komentoriviohjelmia kehitettiin, käyttäjien piti luoda ja tallentaa syötteet erilliseen tiedostoon ja sitten antaa ohjelmalle tiedoston nimi parametrina. Toinen tapa lukea käyttäjän syötteitä on käyttää standardeja tietovirtoja (```stdin``` ja ```stdout```), mutta nämä eivät anna mahdollisuutta ohjelman käytön mukauttamiseen komentorivillä.
Rust-kieltä käytetään usein järjestelmien ohjelmointiin, jossa käyttäjän syötteet ja parametrit voivat olla erittäin vaihtelevia. Siksi komentorivi argumenttien lukeminen on tärkeä osa Rustin kanssa työskentelyä.

## Katso myös:
Rustin dokumentaatio args-vakion käytöstä: https://doc.rust-lang.org/std/env/fn.args.html