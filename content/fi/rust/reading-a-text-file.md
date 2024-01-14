---
title:                "Rust: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi teksti tiedoston lukeminen on tärkeää Rust ohjelmoinnissa? Se on hyödyllistä silloin kun haluat lukea tiedostosta tietoja ja käsitellä niitä ohjelmassasi.

## Miten

Koodiesimerkit ja tulosteesimerkit "```Rust ...```" koodilohkoissa auttavat lukijaa ymmärtämään, miten voit lukea teksti tiedostoja Rust ohjelmoinnissa. 

```Rust

// avaa tiedosto tekstin lukemista varten
let tiedosto = std::fs::File::open(“tiedostonimi.txt”)?;

// luo muuttuja, johon tallennetaan tiedoston sisältö
let sisalto = std::io::BufReader::new(&tiedosto);

// käy läpi tiedoston sisältö rivi kerrallaan
for rivi_tulos in sisalto.lines() {
    let rivi = rivi_tulos?;
    println!("{}", rivi);
}

```

Tulosteesimerkissä käydään läpi tiedoston sisältö rivi kerrallaan ja jokainen rivi tulostetaan konsoliin. Voit myös käsitellä tiedoston sisältöä haluamallasi tavalla, esimerkiksi tallentaa se listaan tai käyttää tietoja johonkin laskutoimitukseen.

## Syvemmälle

Rustilla on monia eri tapoja lukea teksti tiedostoja, kuten käytetty esimerkissä oleva `std::fs` ja `std::io` moduulit. Voit myös käyttää `std::fs::read_to_string` metodia, joka lukee koko tiedoston sisällön yhtenä merkkijonona, tai `std::io::Read` traitia, jota käytetään lukemaan dataa bittien muodossa.

## Katso myös

- [Rust ohjelmointikielen virallinen kotisivu](https://www.rust-lang.org/fi)
- [Rust oppaan lukeminen tiedostosta](https://doc.rust-lang.org/book/ch12-02-reading-a-file.html)
- [Rust standardikirjaston dokumentaatio](https://doc.rust-lang.org/std/index.html)