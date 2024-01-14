---
title:                "Rust: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Rust on tullut suosituksi ohjelmointikielten joukossa sen turvallisuuden, suorituskyvyn ja selkeyden ansiosta. Mutta jokainen ohjelmoija tietää, että joskus täytyy käsitellä ja muokata merkkijonoja. Merkkijonon muuntaminen pieniksi kirjaimiksi (lower case) on yksi hyödyllisimmistä toiminnoista, joita ohjelmoija voi tarvita. Tässä blogikirjoituksessa käymme läpi, kuinka muuntaa merkkijono pieniksi kirjaimiksi käyttäen Rust-kieltä.

## Kuinka

Konvertoitaessa merkkijono pieniksi kirjaimiksi Rust-kielellä, meidän täytyy käyttää `to_lowercase` funktiota. Tämä funktio kuuluu `std::string::String` rakenteeseen ja palauttaa uuden String-tyypin arvon. Tässä on yksinkertainen esimerkki koodista:

```Rust
let s = "Tämä on ESIMERKKI";
let s_lower = s.to_lowercase();
println!("{}", s_lower); // tulostaa: "tämä on esimerkki"
```

Kuten näemme, `to_lowercase` funktio muuntaa merkkijonon `s` pieniksi kirjaimiksi ja palauttaa sen uutena merkkijonona `s_lower` muuttujaan. 

On myös mahdollista muuntaa vain osa merkkijonosta pieniksi kirjaimiksi käyttämällä `to_lowercase` funktiota yhdessä `chars` metodin kanssa. Tässä on esimerkki:

```Rust
let s = "Tämä on ESIMERKKI";
let mut s_chars = s.chars(); // luodaan merkkijonoon s viittaavan merkkilukujonon
println!("{}", s_chars.next().unwrap().to_lowercase()); // tulostaa: "t"
println!("{}", s_chars.as_str()); // tulostaa: "Ämä on ESIMERKKI"
```

Käyttämällä `to_lowercase` funktiota yhdessä `chars` metodin kanssa, pystymme muuntamaan vain ensimmäisen kirjaimen merkkijonossa pieniksi kirjaimiksi ja palauttaa uuden merkkijonon, joka sisältää muuntamattoman osan merkkijonosta.

## Deep Dive

Rustissa merkkijonot ovat muutokselle immuuneja (immutable), mikä tarkoittaa, että jos haluamme muokata alkuperäistä merkkijonoa, meidän täytyy käyttää muunneltavaa (mutable) viitettä (`&mut`). Tämä johtuu Rustin turvallisuusominaisuuksista, jotka estävät pääsyn muokkaamattomiin arvoihin useilta säikeiltä samanaikaisesti. 

On tärkeää huomata, että `to_lowercase` funktio käsittelee merkkijonon Unicode-yhteensopivana. Tämä tarkoittaa, että funktio pystyy käsittelemään erilaisia merkkejä ja kirjaimia eri kielistä oikein. Esimerkiksi, jos merkkijono sisältää saksalaisen ääkkösen `Ä`, funktio muuntaa sen oikein pieneksi kirjaimeksi `ä`.

Lisäksi, Rustin `to_lowercase` ei tue merkkijonoita, jotka sisältävät epäkunnollisia utf-8 merkkejä. Tässä tapauksessa palautetaan virhe.

## Katso myös

- [Rust-kielen virallinen sivusto](https://www.rust-lang.org/fi)
- [Rustin merkkijonon dokumentaatio](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [Rust-kielian uudistuminen ja