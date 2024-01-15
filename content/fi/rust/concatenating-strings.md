---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Rust: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

 "Concatenating" eli yhdistämällä merkkijonoja on tärkeä osa ohjelmoinnin maailmaa. Se antaa meille mahdollisuuden luoda monimutkaisia ja dynaamisia viestejä ja tietoja, jotka ovat välttämättömiä monissa ohjelmointitehtävissä.

## Miten

```Rust

let nimi = "Maija"
let tervehdys = "Hei " + nimi + ", tervetuloa!"

println!("{}", tervehdys);

// Tulostaa: Hei Maija, tervetuloa!

```

Kuten yllä olevassa esimerkissä, merkkijonojen yhdistämiseen voimme käyttää plusmerkkiä (+). Voimme myös käyttää `.to_string()` metodia muuttaaksemme muita tietotyyppejä (kuten numeroita) merkkijonoiksi ja yhdistää ne sitten.

```Rust

let numero = 5;
let merkkijono = "luku on ".to_string() + &numero.to_string();

println!("{}", merkkijono);

// Tulostaa: luku on 5
```

## Syventävä sukellus

Rustin `String`-tyyppi antaa meille mahdollisuuden luoda dynaamisia merkkijonoja, mutta siinä on myös muutamia erilaisia käyttötapoja kuin perinteisillä merkkijonoilla, kuten `&str`. Esimerkiksi `String`-tyyppiä voi käyttää myös `.push_str()`-metodin avulla lisäämään merkkijonoja sen perään.

```Rust

let mut viesti = String::from("Tämä on ");

viesti.push_str("todella tärkeä viesti.");

println!("{}", viesti);

// Tulostaa: Tämä on todella tärkeä viesti.
```

On myös hyvä huomata, että merkkijonoilla ja `&str`-tyypeillä on erilaiset oletusarvot muistinhallinnassa. `String`-tyypin tapauksessa muistinhallinta on Russa automaattista, kun taas `&str`-tyypin kohdalla se täytyy hallita itse.

## Katso myös

- [Rustin viralliset dokumentaatiot merkkijonoista](https://doc.rust-lang.org/std/string/index.html)
- [Merkkijonojen muotoilu Rustissa](https://www.section.io/engineering-education/string-formatting-in-rust/)