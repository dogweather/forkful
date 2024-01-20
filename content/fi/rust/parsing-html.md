---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/parsing-html.md"
---

{{< edit_this_page >}}

(Sorry, but I can't generate text in Finnish.)

## Mitä & Miksi?

HTML -koodin jäsentäminen tarkoittaa sen rakenteen purkamista ohjelmoitavaan muotoon, josta tietokone voi lukea ja käsitellä sisältöä. Tarve siihen voi syntyä erilaisista syistä, kuten web scraping -projekteissa, sivustojen sisällön analysoinnissa tai vaikkapa web-kehittäjän työkaluna sivun debuggauksessa.

## Miten se tehdään:

Asennetaan ensin "html5ever", Rust -kielen HTML parser -kirjasto. 

```Rust
[dependencies]
html5ever = "0.25.1"
```

Tämän jälkeen voidaan luoda yksinkertainen esimerkkikoodi:

```Rust
extern crate html5ever;

use html5ever::driver::ParseOpts;
use html5ever::rcdom::{Doctype, Document, Comment, Element, RcDom, Text};
use html5ever::tendril::TendrilSink;
use html5ever::parse_document;

fn main() {
let html = "<html><head></head><body><h1>Moi Suomi!</h1></body></html>".to_string();
let dom: RcDom = parse_document(RcDom::default(), ParseOpts::default())
.one(html.into());
}
```

Tällä esimerkillä analysoidaan yksinkertainen HTML -rakenne ja tehdään siitä jäsentynyt DOM. 

## Syvempi pureutuminen

HTML -jäsentäminen on ollut ohjelmistotekniikan alalla yleistä sitten webin alkuaikojen, kun tarvittiin keinoja tulkita ja käsitellä kasvavaa määrää web -sisältöä. 

Vaihtoehtoisia tapoja HTML:n jäsentämiseen ovat esimerkiksi erilaisten kirjastojen käyttö muissa ohjelmointikielissä, kuten BeautifulSoup Pythonissa tai Jsoup Javassa. Rust kielen etuina voidaan mainita sen tehokkuus ja turvallisuus.

HTML -jäsentämisen toteutuksessa tärkeitä seikkoja ovat mm. sen kyky käsitellä puutteellisesti muotoiltua tai virheellistä HTML -koodia, sekä sen suorituskyky ja tehokkuus.

## Katso myös:

1. [html5ever documentation](https://docs.rs/html5ever/)
2. [Rust documentation](https://www.rust-lang.org/tools/install)
3. [BeautifulSoup documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
4. [Jsoup documentation](https://jsoup.org/)
5. [Web scraping with Rust](https://kadekillary.work/post/webscraping-rust/)