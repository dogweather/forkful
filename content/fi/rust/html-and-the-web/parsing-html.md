---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:00.379539-07:00
description: "Kuinka: J\xE4sennett\xE4ess\xE4 HTML:\xE4\xE4 Rustilla k\xE4ytet\xE4\
  \xE4n usein `scraper`-pakettia, joka tarjoaa korkean tason rajapinnan HTML-dokumenttien\
  \ l\xE4pik\xE4ymiseen ja\u2026"
lastmod: '2024-03-13T22:44:56.354467-06:00'
model: gpt-4-0125-preview
summary: "J\xE4sennett\xE4ess\xE4 HTML:\xE4\xE4 Rustilla k\xE4ytet\xE4\xE4n usein\
  \ `scraper`-pakettia, joka tarjoaa korkean tason rajapinnan HTML-dokumenttien l\xE4\
  pik\xE4ymiseen ja manipulointiin."
title: "HTML:n j\xE4sennys"
weight: 43
---

## Kuinka:
Jäsennettäessä HTML:ää Rustilla käytetään usein `scraper`-pakettia, joka tarjoaa korkean tason rajapinnan HTML-dokumenttien läpikäymiseen ja manipulointiin.

Lisää ensin `scraper` `Cargo.toml`-tiedostoosi:

```toml
[dependencies]
scraper = "0.12.0"
```

Seuraavaksi on yksinkertainen esimerkki, joka poimii kaikki linkkien URL-osoitteet annetusta HTML-merkkijonosta:

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"
    <html>
    <body>
        <a href="http://example.com/1">Linkki 1</a>
        <a href="http://example.com/2">Linkki 2</a>
    </body>
    </html>
    "#;

    let dokumentti = Html::parse_document(html);
    let valitsin = Selector::parse("a").unwrap();

    for elementti in dokumentti.select(&valitsin) {
        let linkki = elementti.value().attr("href").unwrap();
        println!("Löydetty linkki: {}", linkki);
    }
}
```

Tuloste:

```
Löydetty linkki: http://example.com/1
Löydetty linkki: http://example.com/2
```

Tässä esimerkissä jäsennämme yksinkertaisen HTML-dokumentin löytääksemme kaikki `<a>`-elementit ja poimiaksemme niiden `href`-attribuutit, tehokkaasti tulostaen kaikkien dokumentin linkkien URL-osoitteet. `scraper`-kirjasto yksinkertaistaa HTML:n jäsentämistä ja tiettyjen elementtien valitsemista CSS-valitsimien avulla, tehden siitä suosikin web-skaapauksen tehtävissä Rustissa.
