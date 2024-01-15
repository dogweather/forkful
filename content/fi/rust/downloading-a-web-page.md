---
title:                "Lataaminen verkkosivulta"
html_title:           "Rust: Lataaminen verkkosivulta"
simple_title:         "Lataaminen verkkosivulta"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Miksi
Tiedät varmasti, että internet on täynnä mielenkiintoista sisältöä, jota haluat lukea tai tutkia. Rustin avulla voit luoda ohjelmia, jotka automaattisesti lataavat verkkosivuja puolestasi. Näin voit säästää aikaa ja vaivaa manuaalisilta latauksilta ja keskittyä oleelliseen: sisällön hyödyntämiseen.

# Miten
Koska Rust on nopea ja turvallinen ohjelmointikieli, se sopii hyvin verkkosivujen lataamiseen. Seuraavassa on esimerkki yksinkertaisesta ohjelmasta, joka lataa verkkosivun ja tulostaa sen sisällön konsoliin.

```Rust
use std::io::Read; // Tuodaan tarvittava rajapinta
use reqwest::blocking::Client; // Tuodaan Reqwest-kirjaston rajapinta

fn main() {
    // Luodaan uusi HTTP-asiakas
    let client = Client::new();
    // Ladataan sivu URL-osoitteesta
    let response = client.get("https://example.com").send().unwrap();
    // Lukee vastauksen sisällön ja tallentaa sen muuttujaan
    let mut content = String::new();
    response.read_to_string(&mut content).unwrap();
    // Tulostetaan sisältö konsolissa
    println!("{}", content);
}
```

Tämä yksinkertainen ohjelma käyttää Reqwest-kirjastoa, joka tarjoaa helpon tavan ladata verkkosivuja Rustissa. Koodin lukeminen, jäljittäminen ja suorittaminen on yksinkertaista ja turvallista.

# Syväsukellus
Verkkosivujen lataaminen on tärkeä osa monia ohjelmia, ja Rust tarjoaa siihen lukuisia työkaluja ja kirjastoja. Esimerkiksi Reqwest-kirjasto tarjoaa myös mahdollisuuden tehdä asynkronista verkkolatausta, mikä tarkoittaa että useita verkkosivuja voidaan ladata samanaikaisesti ilman tarvetta odottaa edellisen latauksen valmistumista.

Rustin mukana tuleva `std::process` -kirjasto tarjoaa mahdollisuuden suorittaa komentoriviltä muita ohjelmia, kuten wget tai cURL, verkkosivujen lataamiseen tarvittavien työkalujen avulla. Näissä tapauksissa Rust toimii vain käytännöllisenä rajapintana näiden työkalujen käyttämiseen.

# Katso myös
- [Rustin virallinen verkkosivu](https://www.rust-lang.org)
- [Reqwest-kirjaston dokumentaatio](https://docs.rs/reqwest)
- [Rustin standardikirjaston dokumentaatio](https://doc.rust-lang.org/std/index.html)