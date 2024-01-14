---
title:    "Rust: Luodaan väliaikainen tiedosto"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Miksi luoda väliaikainen tiedosto?

Monissa ohjelmointiprojekteissa saattaa tulla tarve luoda väliaikainen tiedosto. Tämä voi johtua esimerkiksi tietojen tallentamisesta, puskuroinnista tai muista teknisistä syistä. Rustin avulla tämä prosessi voidaan tehdä nopeasti ja turvallisesti. Seuraavassa esittelemme kuinka väliaikaisten tiedostojen luonti onnistuu Rustilla.

## Kuinka luoda väliaikainen tiedosto

Väliaikaisten tiedostojen luonti Rustilla on helppoa. Käytämme siihen Rustin standardikirjaston `tempfile`-kirjastoa. Alla esimerkki koodista, joka luo väliaikaisen tiedoston ja kirjoittaa siihen "Tervetuloa Rust-maailmaan!".

```Rust
use std::io::Write;
use tempfile::NamedTempFile;

fn main() {
    // Luodaan väliaikainen tiedosto nimellä "tervetuloa.txt"
    let mut file = NamedTempFile::new("tervetuloa.txt").unwrap();

    // Kirjoitetaan teksti tiedostoon
    writeln!(file, "Tervetuloa Rust-maailmaan!").unwrap();

    // Näytetään tiedoston polku
    println!("Tiedoston polku: {}", file.path().display());
}
```

Sama koodi suoritettuna tulostaa seuraavan:

```
Tiedoston polku: /tmp/tervetuloa.txt
```

Kun ohjelma sulkeutuu, väliaikainen tiedosto poistuu automaattisesti.

## Syvempi sukellus

`tempfile`-kirjasto tarjoaa monia erilaisia toimintoja väliaikaisten tiedostojen hallintaan. Voit esimerkiksi määrittää tiedoston sijainnin, nimen ja jopa laajennoksen. Alla oleva koodi luo väliaikaisen tiedoston nimellä "tervetuloa.png" ja tallentaa sen sijaintiin `/tmp/kuvat`.

```Rust
use tempfile::Builder;

fn main() {
    // Luodaan väliaikainen tiedosto nimellä "tervetuloa.png" sijaintiin "/tmp/kuvat"
    let file = Builder::new()
        .prefix("tervetuloa")
        .suffix(".png")
        .tempfile_in("/tmp/kuvat")
        .unwrap();

    // Näytetään tiedoston polku
    println!("Tiedoston polku: {}", file.path().display());
}
```

Tämä koodi printtaa:

```
Tiedoston polku: /tmp/kuvat/tervetuloaab18e8f4-1c33-4705-9a41-4f97826cecce.png
```

Halutessasi voit myös säätää tiedoston oikeuksia ja sen käyttömahdollisuuksia `tempfile::Builder`-olion avulla.

## Katso myös

- [tempfile-kirjaston dokumentaatio](https://docs.rs/tempfile/)
- [Rustin virallinen opas](https://rust-lang.github.io/rustup/index.html)
- [Tempfile-kirjaston lähdekoodi GitHubissa](https://github.com/Stebalien/tempfile)

Kiitos kun luit tämän artikkelin ja toivottavasti se auttoi sinua ymmärtämään väliaikaisten tiedostojen luontia Rustilla! Onnea ohjelmointiin.