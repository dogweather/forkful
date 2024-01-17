---
title:                "HTML-analysaatio"
html_title:           "Rust: HTML-analysaatio"
simple_title:         "HTML-analysaatio"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/parsing-html.md"
---

{{< edit_this_page >}}

Mikä & Miksi?
HTML:n jäsentäminen on prosessi, jossa ohjelmoijat analysoivat HTML-koodin rakennetta ja sisältöä. Tämä on tärkeää, koska se mahdollistaa tiedon hakemisen ja muokkaamisen tiettyjä sivustoja varten. Esimerkiksi web-sivujen skannaaminen ja tietojen kerääminen voi usein vaatia HTML-jäsentämistä.

Miten:
```rust
Käytä crate html5ever;
Käytä std::io;

fn main() {
    // Alusta HTML-jäsennin
    let jäsennin = html5ever::parse_document(
        html5ever::rcdom::RcDom::default(),
        Default::default()
    ).from_utf8()
        .read_from(&mut io::Cursor::new(html_as_bytes()))
        .unwrap();

    // Tulosta otsikot HTML-sivulta
    for lapsi in jäsentin.nodet() {
        jos lapsi.nimi() == "otsikko" {
            println!("{}: {}", lapsi.langattu(), lapsi.lasts());
        }
    }
}
```

Tässä esimerkissä käytetään "html5ever" crateä HTML-jäsennykseen. Crate asennetaan cargo-työkalulla komennolla ```cargo install html5ever```. Tämän jälkeen ```std::io``` cratea käytetään HTML-sivun lukemiseen ja käsittelijä luodaan käyttämällä html5everin toimintoja. Lopuksi tulostetaan sivun otsikot.

Syväallas:
HTML-jäsentämistä on käytetty jo 1990-luvulta lähtien ja siitä on tullut tärkeä osa web-kehitystä. Se on monipuolinen ja tehokas tapa hakea ja muokata tietoa eri sivustoilla. Rustilla on useita HTML-jäsennin crateja, kuten myös muita vaihtoehtoja kuten Javascript-pohjaisia ratkaisuita.

Katso myös:
- [html5ever crate dokumentaatio](https://docs.rs/html5ever/0.23.0/html5ever/)
- [Rust-ohjelmoinnin aloittaminen](https://www.rust-lang.org/learn/get-started)
- [Javascript-pohjainen HTML-jäsennin](https://github.com/cheeriojs/cheerio)