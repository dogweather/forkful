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

## Hva & Hvorfor?
Å "parse" HTML er grenen av programmering som er ansvarlig for å analysere og tolke HTML-kode. Dette er en viktig ferdighet for utviklere som ønsker å lage nettsted eller apper som bruker HTML som sitt grunnleggende språk. Ved å kunne "parse" HTML-kode, kan programmerere enkelt trekke ut data fra nettsted og manipulere det på ønsket måte.

## Hvordan:
Her er et eksempel på hvordan du kan bruke Rust for å "parse" HTML-kode:

```Rust
extern crate select;

use select::document::Document;
use select::predicate::Name;

fn main() {
    let document = Document::from("<p>Hello, <span>world!</span></p>");
    for node in document.find(Name("span")) {
        println!("Found: {}", node.text());
    }
}
```
Eksempel utdata:
```
Found: world!
```

## Dykk ned:
Parsing av HTML-kode har vært en del av programmering siden internettets begynnelse. I starten var det vanlig å bruke regex (regulære uttrykk) for å analysere og manipulere HTML-kode. Dette var imidlertid vanskelig og skjørt, og derfor har det blitt utviklet mange verktøy og biblioteker for å gjøre parsing enklere og mer pålitelig.

Alternativt til å bruke Rust for å parse HTML, er det programmeringsspråk som Python og JavaScript som også har gode verktøy for dette. I Rust er biblioteket "select" et populært valg for parsing av HTML-kode. Det er også verdt å nevne at noen utviklere foretrekker å bruke HTML-parsere som allerede er integrert i deres utviklingsmiljø, som for eksempel med Django i Python.

Når det gjelder implementasjon, bruker Rust vanligvis en kombinasjon av "select" biblioteket og ["nom"](https://github.com/Geal/nom) for å behandle HTML-strukturen og trekke ut data. Verktøy som "select" gjør det enkelt å finne spesifikke HTML-elementer ved å bruke kriterier som tag-navn, klassenavn, og ID.

## Se også:
- [Offisiell Rust dokumentasjon for HTML parsing](https://www.rust-lang.org/learn/get-started)
- [Rust "select" biblioteket](https://github.com/utkarshkukreti/select.rs)
- [Alternativt å bruke HTML-parsere i Python](https://wiki.python.org/moin/HTML)
- ["nom" biblioteket for parsing og analysing](https://github.com/Geal/nom)