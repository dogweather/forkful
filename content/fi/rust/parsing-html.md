---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:34:04.466148-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
HTML:n jäsentäminen tarkoittaa HTML-koodin rakenteen muuttamista sellaiseen muotoon, johon ohjelmat voivat helposti kajota. Ohjelmoijat jäsentävät HTML:ää datan kaivamiseksi verkkosivuilta, automaatioon tai sisällön validointiin.

## How to: (Kuinka tehdä:)
```Rust
extern crate select;
use select::document::Document;
use select::predicate::Name;

fn main() {
    let html = r#"
        <html>
            <head>
                <title>Rust Esimerkki</title>
            </head>
            <body>
                <h1>Tervetuloa!</h1>
                <p>Rust ja HTML jäsentäminen.</p>
            </body>
        </html>
    "#;

    let document = Document::from(html);

    let title = document.find(Name("title")).next().unwrap().text();
    println!("Otsikko: {}", title);

    let header = document.find(Name("h1")).next().unwrap().text();
    println!("Otsake: {}", header);
}
```
Sample output:
```
Otsikko: Rust Esimerkki
Otsake: Tervetuloa!
```

## Deep Dive (Sukellus syvyyksiin):
HTML:n jäsentäminen ei ole uusi konsepti; se on ollut web-kehityksen perusta jo vuosikymmenien ajan. Rust-ekosysteemissä käytettävät kirjastot, kuten `select`, tarjoavat tehokkaat työkalut HTML:n jäsentämiseen. Vaihtoehtoina on muita kirjastoja kuten `html5ever`, joka perustuu modernin HTML:n syntaksianalyysiin ja `scraper`, joka puolestaan tukeutuu `select`-kirjastoon.

Nämä kirjastot helpottavat HTML-elementtien valikointia ja niiden tietojen manipulointia, esimerkiksi CSS-selektorien avulla. Jäsentämisen suorituskyky ja tarkkuus ovat Rustissa keskeisiä etuja, erityisesti concurrent ja memory-safe suunnitteluperiaatteiden ansiosta.

## See Also (Katso myös):
- `select`-kirjaston dokumentaatio: https://docs.rs/select/latest/select/
- `html5ever` GitHub-sivu: https://github.com/servo/html5ever
- `scraper`-kirjaston dokumentaatio: https://docs.rs/scraper/latest/scraper/
- Rust-ohjelmointikielen viralliset oppaat: https://doc.rust-lang.org/book/
- Rust-käyttäjäkokouksen esitelmät HTML:n jäsentämisestä: https://www.rust-lang.org/community#conferences