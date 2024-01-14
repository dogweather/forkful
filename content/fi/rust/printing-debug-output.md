---
title:                "Rust: Debug-tulostus"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet ohjelmointimaailmassa läsnä, on hyvin todennäköistä, että olet törmännyt debuggaamiseen, eli virheiden etsimiseen ohjelmakoodista. Yksi tehokas tapa helpottaa virheen jäljittämistä on tulostaa debug-tekstiä, joka antaa sinulle tärkeää tietoa ohjelman suorituksesta. Tässä blogikirjoituksessa käsittelemme, miten voit käyttää Rust-ohjelmointikielen debug-ominaisuuksia ja millaisia etuja se tarjoaa.

## Miten

Rustilla on sisäänrakennettu debug-toiminto `println!()`, joka tulostaa tekstin konsoliin. Voit käyttää sitä tulostamaan muuttujien arvoja ja muita tietoja ohjelman suorituksen aikana. Alla on esimerkki:

```Rust
let nimi = "Mikko";
let ikä = 24;
println!("Terve, minun nimeni on {} ja olen {} vuotta vanha.", nimi, ikä);
```

Tämä tulostaisi seuraavan tekstin:

```
Terve, minun nimeni on Mikko ja olen 24 vuotta vanha.
```

Voit myös käyttää `dbg!()` makroa, joka tulostaa sekä muuttujan nimen että arvon. Tämä on erityisen hyödyllinen, kun haluat tarkistaa, onko muuttujan arvo oikea. Esimerkki:

```Rust
let luku = 42;
dbg!(luku);
```

Tämä tulostaisi:

```
[luku: 42]
```

## Syvempi sukellus

Molemmat `println!()` ja `dbg!()` voivat myös ottaa vastaan monia argumentteja, jotta voit muotoilla tulostettavaa tekstiä haluamallasi tavalla. Voit esimerkiksi käyttää muotoilija `%` merkinnällä, joka vastaa C-kielen tunnettua `printf` funktiota.

Voit myös rajata missä paketissa debug-teksti tulostetaan, lisäämällä `#[cfg(debug_assertions)]` ennen `println!()` tai `dbg!()` riviä. Tämä varmistaa, että debug-teksti näkyy vain debug-tarkoituksissa eikä vaikuta ohjelman suoritukseen.

On myös hyvä pitää mielessä, että kaikki debug-teksti lisää ohjelman kokoa ja hidastaa sitä hieman, joten on tärkeää poistaa tai disabloida debug-rivit ennen ohjelman julkaisua.

## Katso myös

- [Rustin dokumentaatio debuggauksesta](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
- [Rust-ohjelmointikielen kotisivu](https://www.rust-lang.org/fi/)
- [Debuggaus vinkkejä Rust ohjelmille](https://crates.io/crates/crate-template)