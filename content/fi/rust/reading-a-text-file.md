---
title:                "Tiedoston lukeminen"
html_title:           "Rust: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Lukeminen tekstitiedostosta tarkoittaa yksinkertaisesti tekstin hakemista ja näyttämistä ohjelman koodissa. Ohjelmoijat tekevät tätä esimerkiksi käsitellessään suuria tietomääriä tai tallentaessaan käyttäjän antamia tietoja.

## Miten:
Esimerkiksi voidaan käyttää Rustin sisäänrakennettua “File”-moduulia, joka mahdollistaa tiedostojen avaamisen ja lukemisen. Käytännössä tämä tapahtuu seuraavasti:
```Rust
let tiedosto = File::open("tiedosto.txt")?;
```
Voidaan myös käyttää “BufReader”-moduulia, joka lisää pieniä optimointeja tiedoston lukemiseen. Alla esimerkki käyttäen “BufReader”-moduulia:
```Rust
let tiedosto = File::open("tiedosto.txt")?;
let buffer = BufReader::new(tiedosto);
```

## Syväsukellus:
Tiedoston lukeminen on ollut tärkeä osa ohjelmointia jo kauan. Ennen tekstitiedostojen lisääntynyttä suosiota käytettiin erityisiä tietosisältömuotoja, kuten CSV tai XML. Näitä formaatteja käytetään edelleen tänäkin päivänä, mutta tekstitiedostot ovat yhä suositumpi vaihtoehto yksinkertaisempien tietojen tallentamiseen.

Mikäli haluat käsitellä suuria tietomääriä, kannattaa harkita muiden formaattien käyttöä, kuten tietokantoja tai JSON-tiedostoja. Tekstitiedostoilla on myös rajoituksensa, kuten tiedostokoon rajat ja tietosisältöön liittyvät haasteet.

## Katso myös:
- [Rustin "File" moduulin dokumentaatio](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rustin "BufReader" moduulin dokumentaatio](https://doc.rust-lang.org/std/io/struct.BufReader.html)
- [Tietokantaohjelmointi Rustilla](https://medium.com/@mehrdad/learn-database-development-in-100-minutes-now-with-rust-68f9a0b4c433)