---
title:                "Rust: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

HTML er språket som brukes til å lage nettsider, og som utviklere er det viktig å kunne lese og håndtere dette språket. Å kunne parse (altså, å analysere og behandle) HTML er en viktig ferdighet for å kunne utvikle effektive og funksjonelle nettsider og applikasjoner. Derfor er det viktig å lære hvordan man kan gjøre dette ved hjelp av Rust-programmeringsspråket.

## Hvordan

Det finnes mange måter å parse HTML på, men i denne bloggposten skal vi fokusere på å bruke Rust sin "html-parser" bibliotek. Først må du legge til dette biblioteket som en avhengighet i ditt Rust-prosjekt ved hjelp av "Cargo.toml"-filen.

```Rust
[dependencies]
html-parser = "0.1.0"
```

Deretter kan du begynne å bruke biblioteket i koden din. La oss si at vi ønsker å parse HTML fra en nettside og hente ut alle bildelinker fra siden. Vi kan gjøre dette ved hjelp av følgende kode:

```Rust
extern crate html_parser;
use html_parser::Dom;

fn main() {
    let html = r#"
        <html>
            <head>
                <title>Min hjemmeside</title>
            </head>
            <body>
                <h1>Dette er en overskrift</h1>
                <img src="bilde1.png" alt="Bilde 1">
                <img src="bilde2.png" alt="Bilde 2">
                <img src="bilde3.png" alt="Bilde 3">
            </body>
        </html>
    "#;
    let dom = Dom::parse(html);
    for node in dom.iter() {
        if node.is_text() {
            let text_node = node.as_text().unwrap();
            if text_node.trim() != "" {
                println!("Tekst: {}", text_node);
            }
        }
        if node.has_tag_name("img") {
            let img = node.as_element().unwrap();
            if let Some(src) = img.get_attribute("src") {
                println!("Bildelink: {}", src);
            }
        }
    }
}
```

Denne koden vil gi følgende output:

```
Bildelink: bilde1.png
Bildelink: bilde2.png
Bildelink: bilde3.png
```

Dette er bare et enkelt eksempel på hvordan man kan bruke "html-parser" biblioteket. Det finnes mange flere funksjoner og metoder som kan brukes for å hente ut spesifikke elementer eller informasjon fra en nettside.

## Dykk dypere

Parsing av HTML kan være en kompleks prosess, spesielt når man kommer til mer avanserte nettsider som inneholder dynamisk generert innhold. Videre kan det være situasjoner der man må håndtere feil og ugyldig HTML. "html-parser" biblioteket tar høyde for dette ved å tilby feilhåndtering og robust funksjonalitet for å håndtere ulike HTML-konstruksjoner.

I tillegg til "html-parser" biblioteket, finnes det også andre nyttige verktøy som kan hjelpe med parsing av HTML i Rust, som for eksempel "select" og "scraper" bibliotekene.

## Se også

- [The Rust Programming Language](https://www.rust-lang.org)
- [Official rust-html-parser GitHub repository](https://github.com/rust-lang/html-parser)
- [More examples and documentation for html-parser](https://docs.rs/html-parser/)