---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Alimerkkijonojen erottaminen tarkoittaa merkkijonon osan poimimista tiettyjen ohjelmien toiminnan kannalta olennaiseksi. Se auttaa käsittelyssä ja analysoinnissa koodin suorituskyvyn optimoinnissa.

## Näin teet:

Rustin `slice`-toiminto on yksi yleisimmästi käytetyistä alimerkkijonojen erottamiseen:

```Rust
fn main() {
    let s = "Tervetuloa Rustiin!";
    let tervetuloa = &s[0..11];
    println!("{}", tervetuloa);
}
```

Tämän koodin ajo tuottaa:
```
Tervetuloa
```
On myös mahdollista jättää indeksit pois tietyissä tapauksissa, kuten:

```Rust
fn main() {
    let s = "Tervetuloa Rustiin!";
    let alku = &s[..11];
    let loppu = &s[11..];
    println!("{}{}", alku, loppu);
}
```
Tämä tulostaa alkuperäisen merkkijonon kokonaisuudessaan:
```
Tervetuloa Rustiin!
```

## Syvempi katsaus:

Rustin `slice`-operaatiot ovat peräisin alkuperäisen C-ohjelmointikielen ojelmointikielen osajonojen käsitteestä. Rustissa osajonojen erottamiseksi on myös muita keinoja, kuten `str::split_at`-funktio, joka voi olla hyödyllinen, kun työskentelet suurempien merkkijonodatan kanssa.

```Rust
fn main() {
    let s = "Tervetuloa Rustiin!";
    let (tervetuloa, loppu) = s.split_at(11);
    println!("{}", tervetuloa);
}
```
Tämä esimerkki tulostaa `Tervetuloa`.

## Katso myös:

Rustilla on runsaasti dokumentaatioita ja lähdemateriaalia, joka tarjoaa syvempää tietoa ja esimerkkejä alimerkkijonojen erottamisesta. Tässä muutama linkki, jotka kannattaa tutkia:

- Rustin virallinen dokumentaatio: [String Slices](https://doc.rust-lang.org/book/ch04-03-slices.html)
- Stackoverflow: [How do I create a substring in Rust?](https://stackoverflow.com/questions/25428920/how-do-i-create-a-substring-in-rust) 
- Rust-lang GitHub: [The Rust Programming Language](https://github.com/rust-lang/rust)