---
title:                "HTML:n jäsennys"
date:                  2024-02-03T19:13:00.379539-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML:n jäsennys"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

HTML:n jäsentäminen Rustilla koskee tietojen poimimista HTML-dokumenteista, mikä on olennaista web-skaapaukselle, datan poiminnalle tai web-kiipijöiden rakentamiselle. Ohjelmoijat tekevät tämän automatisoidakseen tiedonkeruun verkosta, analysoidakseen web-sisältöä tai siirtääkseen sisältöä alustalta toiselle.

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
