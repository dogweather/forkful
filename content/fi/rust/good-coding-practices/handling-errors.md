---
date: 2024-01-26 00:59:38.034803-07:00
description: "Virheenk\xE4sittely on toimien hallintaa, kun asiat eiv\xE4t mene odotetusti.\
  \ Ohjelmoijat tekev\xE4t sit\xE4 k\xE4sitell\xE4kseen odottamattomia tilanteita,\
  \ varmistaakseen,\u2026"
lastmod: '2024-03-13T22:44:56.364174-06:00'
model: gpt-4-1106-preview
summary: "Virheenk\xE4sittely on toimien hallintaa, kun asiat eiv\xE4t mene odotetusti.\
  \ Ohjelmoijat tekev\xE4t sit\xE4 k\xE4sitell\xE4kseen odottamattomia tilanteita,\
  \ varmistaakseen,\u2026"
title: "Virheiden k\xE4sittely"
weight: 16
---

## Mikä & Miksi?

Virheenkäsittely on toimien hallintaa, kun asiat eivät mene odotetusti. Ohjelmoijat tekevät sitä käsitelläkseen odottamattomia tilanteita, varmistaakseen, että heidän Rust-ohjelmansa ovat kestäviä eivätkä vain kaadu kohtaessaan pikkuongelman.

## Miten:

Rust käsittelee virheitä kahdella pääasiallisella tavalla: palautettavissa olevat ja palautumattomat virheet. Katsoaanpa molempia.

Palautettavissa olevat virheet käyttävät `Result<T, E>`:

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("Tiedosto avattu onnistuneesti."),
        Err(_e) => println!("Tiedoston avaaminen epäonnistui."),
    }
}
```

Tuloste voi olla joko "Tiedosto avattu onnistuneesti." tai "Tiedoston avaaminen epäonnistui.", riippuen `hello.txt` tiedostostasi.

Palautumattomiin virheisiin käytämme `panic!`:

```Rust
fn main() {
    // Tämä aiheuttaa ohjelman kaatumisen, koska tiedosto todennäköisesti ei ole olemassa.
    let _f = File::open("nowhere.txt").unwrap();
}
```

Aja se, ja näet paniikkiviestin. Ohjelmasi pysähtyy kuin seinään.

## Syväsukellus

Historiallisesti virheenkäsittely ohjelmoinnissa on ollut sotkuista. Rust saa sen oikein selkeällä erolla palautettavissa olevien ja palautumattomien virheiden välillä.

`Result` enum on tarkoitettu palautettavissa oleville virheille. Se on eksplisiittinen – käsittelet `Ok` tai `Err` variantin. Sinulla on myös metodeja kuten `unwrap()` ja `expect()`, mutta ne ovat nopeita ja likaisia oikoteitä, jotka voivat johtaa `panic!`-tilaan.

`panic!` on Rustin tapa huutaa, että jotakin todella pahaa tapahtui, eikä se pysty käsittelemään sitä. Se on kuin palautumaton virhe, joka pysäyttää suorituksen välittömästi. Panikki Rustissa koetaan usein virheinä, joita ei odoteta käsiteltävän, kuten taulukon rajojen ulkopuolelle indeksoinnissa.

Virheenkäsittely `Result`-palautuksella on suositeltavaa, kun odotat käsitteleväsi virheitä. Se on idiomaattista Rustia, mikä tarkoittaa, että se on tapa, josta Rust-kehittäjät ovat sopineet. On myös `Option<T>` tapauksia varten, kun virhe on vain se, että jokin on `None` eikä `Some(T)`. On kyse odottamattoman odottamisesta ilman pelkoa.

Vaihtoehtoja? Toki, voisit käyttää muita virheenkäsittelyn kirjastoja saadaksesi lisää ominaisuuksia tai ergonomista käyttöä. Kuten `anyhow` yksinkertaiseen virheenkäsittelyyn tai `thiserror` kirjastokoodin virheisiin.

## Katso myös

Kiinnostunut sukeltamaan syvemmälle? Tässä minne mennä:

- [Rust-kirja virheenkäsittelystä](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - Loistava paikka ymmärtää Rustin virheenkäsittelyn filosofia.
- [Rust by Example: Error handling](https://doc.rust-lang.org/rust-by-example/error.html) - Interaktiivisia esimerkkejä, jotka saavat sinut tarttumaan toimeen.

Muista, hyvä virheenkäsittely ei ole vain ohjelmointia; se on välittämistä koodisi käyttäjistä. Iloista koodausta!
