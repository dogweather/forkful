---
title:                "Rust: Verkko-sivun lataaminen"
simple_title:         "Verkko-sivun lataaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Rust on moderni ja tehokas kieli, joka tuo uudenlaisia mahdollisuuksia ohjelmointiin. Web-sivujen lataaminen on yksi esimerkki, miten Rustin avulla voi kehittää tehokkaita ja luotettavia ohjelmia.

## Kuinka tehdä

```Rust
fn main() {
    // Ladataan web-sivu
    let response = reqwest::get("https://www.example.com/pdf/file.pdf")
        .await.expect("Failed to make request");

    // Luodaan uusi tiedosto ja tallennetaan web-sivun sisältö sinne
    let mut file = File::create("file.pdf").unwrap();
    file.write_all(&response.bytes().await.unwrap()).unwrap();

    println!("Web-sivu ladattiin onnistuneesti!");
}
```

## Syvemmälle

Web-sivun lataaminen Rustilla mahdollistaa monipuoliset mahdollisuudet tiedonhallintaan. Koodin avulla voidaan esimerkiksi lukea sivulta tietoja ja tallentaa ne tietokantaan tai käsitellä niitä muilla tavoin.

## Katso myös

- [Rust-kielen viralliset verkkosivut](https://www.rust-lang.org/)
- [Reqwest-kirjaston dokumentaatio](https://docs.rs/reqwest/0.11.4/reqwest/)
- [Esimerkkejä web-sivujen lataamisesta Rustilla](https://github.com/search?q=download+web+page+Rust&type=Repositories)