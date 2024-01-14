---
title:                "Rust: Html-tiedostojen jäsentäminen"
simple_title:         "Html-tiedostojen jäsentäminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Miksi käyttäisit lämpötilaohjelmoitua kieltä kuten Rustia parsimaan HTML-koodia? HTML on lähes jokaisen verkkosivun perusta ja sen käsittely on usein välttämätöntä web-kehittäjille. Rustin tehokkuus, nopeus ja turvallisuus tekevät siitä ihanteellisen kielen HTML:n parsimiseen.

## Miten

Rustilla on monia hyödyllisiä työkaluja HTML:n parsimiseen, kuten kattava kirjasto nimeltään `html-parser`. Tässä esimerkissä käytämme tätä kirjastoa yksinkertaisen HTML-dokumentin parsimiseen ja sen sisältämien linkkien tulostamiseen.

```Rust
use html_parser::{Dom, Result};

fn main() -> Result<()> {
    // Määritetään HTML-dokumentti merkkijonona
    let html = r#"
    <html>
        <head>
            <title>Rust Blogi</title>
        </head>
        <body>
            <h1>Tervetuloa lukemaan Rustia!</h1>
            <p>Tässä on muutamia hyödyllisiä linkkejä:</p>
            <ul>
                <li><a href="https://rust-lang.org">Rustin virallinen sivusto</a></li>
                <li><a href="https://github.com/rust-lang/rust">Rustin GitHub-repositorio</a></li>
                <li><a href="https://learncs.org">Rustin oppimateriaali</a></li>
            </ul>
        </body>
    </html>
    "#;

    // Parsitaan HTML ja tallennetaan tulos muuttujaan `dom`
    let dom = Dom::parse(html)?;

    // Haetaan tagit, jotka sisältävät linkkejä ja tulostetaan niiden `href`-arvot
    for link in dom.find("a") {
        println!("{}", link.attributes.get("href").unwrap());
    }

    Ok(())
}
```

Tämä koodi tulostaa seuraavan:

```
https://rust-lang.org
https://github.com/rust-lang/rust
https://learncs.org
```

## Syvemmälle

HTML:n parsiminen kiinnostuneille löytyy runsaasti erilaisia kirjastoja ja työkaluja. Rustilla on myös muita vaihtoehtoja, kuten `scraper`, joka tarjoaa erilaisia tapoja hakea ja muokata HTML-tietoja.

On myös hyödyllistä tutustua HTML:n rakenteeseen ja erilaisiin elementteihin, jotta osaa etsiä ja parsia haluamiaan tietoja. Esimerkiksi `html-parser`-kirjasto mahdollistaa tiettyjen tagien, kuten `a` linkkien, hakemisen ja niiden sisältämän `href`-arvon käyttämisen.

## Katso myös

- [Html-paketti Rust-kielessä](https://docs.rs/html-parser/0.7.0/html_parser/)
- [Scraper-paketti Rust-kielessä](https://docs.rs/scraper/0.12.0/scraper/)
- [HTML-opas](https://www.w3schools.com/html/default.asp)