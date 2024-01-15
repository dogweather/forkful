---
title:                "Analysering av HTML"
html_title:           "Rust: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Å analysere HTML er en viktig del av å bygge nettsider og applikasjoner, og det er også et område der Rust-skillet virkelig skinner. Ved å bruke Rust for å analysere HTML, kan du dra nytte av språkets sterke typer og sikkerhet, noe som kan redusere feil og gjøre koden din mer pålitelig.

## Hvordan

La oss se på et enkelt eksempel på parsing av HTML ved hjelp av Rust. Først må vi importere biblioteket "html-parse", som er tilgjengelig på crates.io:

```Rust
extern crate html_parse;
```

Nå kan vi bruke funksjonen "parse" for å analysere en HTML-streng og få ut et strukturert tre av elementene:

```Rust
let html_string = String::from("<div><p>Hello, world!</p></div>");
let dom = html_parse::parse(&html_string);
```

Vi kan deretter gå gjennom treet og hente ut informasjonen vi trenger. For eksempel, hvis vi vil hente ut teksten inni på-elementet, kan vi gjøre det som følger:

```Rust
if let Some(ref node) = dom.children[0].children[0] {
    if let HtmlNode::Element(ref element) = *node.borrow() {
        // Sjekk om dette er et <p> element
        if element.tag_name == "p" {
            // Hent ut teksten inni <p> elementet
            let text = element.children[0].text.clone();
            // Skriv ut teksten
            println!("{}", text);
        }
    }
}
```

Dette vil skrive ut "Hello, world!" til konsollen.

## Dypdykk

Rust-biblioteket "html-parse" bruker et konsept kalt "borrowing" for å sikre at det ikke blir gjort flere "owners" av datastrukturen samtidig, noe som kan føre til låser. Dette er et viktig aspekt ved Rust og hvordan det håndterer eierskap til data. Det finnes også andre biblioteker for å analysere HTML i Rust, som "scraper" og "select", som kan være nyttige avhengig av behovene dine.

## Se også

- [Official Rust website](https://www.rust-lang.org/)
- [Crate for html-parse](https://crates.io/crate/html-parse)
- [Crate for scraper](https://crates.io/crate/scraper)
- [Crate for select](https://crates.io/crate/select)