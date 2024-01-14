---
title:    "Rust: Komentoriviparametrien lukeminen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Miksi lukisin komentoriviparametreja? Ensinnäkin, lukemalla komentoriviparametreja pystytään antamaan ohjelmalle tarkempia ja monipuolisempia ohjeita sen suorittamiseen. Tämä on erityisen hyödyllistä silloin, kun halutaan suorittaa sama ohjelma erilaisilla asetuksilla.

## Kuinka

Komentoriviparametrien lukeminen Rustilla on helppoa ja suoraviivaista. Alla on esimerkki, jossa luodaan pieni ohjelma, joka tulostaa annetun nimen.

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        println!("Hei, {}!", args[1]);
    } else {
        println!("Hei maailma!");
    }
}
```

Oletetaan, että koodi on tallennettu tiedostoon nimeltä `tervehdys.rs`. Voimme suorittaa ohjelman antamalla komentorivillä esimerkiksi seuraavanlaisen komennon:

```
rustc tervehdys.rs
./tervehdys Jesse
```

Tämän tuloksena ohjelma tulostaa:

```
Hei, Jesse!
```

Jos taas komentorivillä ei anneta nimeä, ohjelma tulostaa:

```
Hei maailma!
```

Koodissa käytetään `std::env` kirjastoa, joka tarjoaa toiminnallisuuden komentoriviparametrien lukemiseen. Tässä tapauksessa yksi komentoriviparametri tallennetaan `args` vektoriin. Jos annettuja parametreja on enemmän kuin yksi, tulostetaan ensimmäinen parametri `args` vektorista. Muussa tapauksessa tulostetaan oletusviesti.

## Syventävä tarkastelu

Komentoriviparametrit voivat olla hyödyllisiä monessa eri tilanteessa. Esimerkiksi ohjelman käyttöä voidaan muokata riippuen annetuista parametreista, tai ohjelma voi tulostaa lisätietoa tietyn parametrin avulla. Rust tarjoaa paljon erilaisia tapoja lukea komentoriviparametreja ja niiden käsittelyyn on monia erilaisia lähestymistapoja.

## Katso myös

- [Rustin virallinen dokumentaatio komentoriviparametrien lukemisesta](https://doc.rust-lang.org/std/env/fn.args.html)
- [Rustin ohjelmointikielen opas](https://github.com/github.com/jmajava/ohjelmointikielet/blob/master/rust)
- [Komentoriviparametrit Tietokoneen perusteet -kurssilla](https://painovoima.org/ruben/cli-argumentit.html)