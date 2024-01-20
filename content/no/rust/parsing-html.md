---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse HTML handler om å tolke en HTML-fil og bygge et tre av noder for hvert HTML-element som kan leses og manipuleres av en programmerer. Vi gjør dette for å hente ut, endre, eller navigere informasjon på en nettside.

## Slik gjør du det:

Bruk `scraper`-pakken i Rust til å parse HTML. Installer med `cargo`:

```Rust
[dependencies]
scraper = "0.3.1"
```

Kodeeksempel:

```Rust
use scraper::{Html, Selector};

fn main() {
    let html = Html::parse_document("<div><h1>Hei, Norge!</h1></div>");

    let selector = Selector::parse("h1").unwrap();

    let element = html.select(&selector).next().unwrap();

    println!("{:?}", element.inner_html()); // Printer: "Hei, Norge!"
}
```
Kjører du denne koden vil du se "Hei, Norge!" i terminalen.

## Dypdykk

Å parse HTML har interessant historisk kontekst. I begynnelsen, tidlig på 1990-tallet, var web-sider ganske enkle, men de har vokst i kompleksitet. Dagens behov for enkel navigering og manipulering av HTML har ført til utviklingen av mange kjente parsing bibliotek.

Det er alternative måter å parse HTML på i Rust, som `html5ever` og `kuchiki`, men `scraper` er vanligvis det beste valget for nybegynnere på grunn av enklere syntaks og brukervennlighet. 

HTML parsing er basert på byggingen av en Document Object Model (DOM). Når en HTML-fil blir parset, blir hvert HTML-element omformet til en node i DOM-treet, deretter kan nodenes tekst, attributter og tilknyttede noder manipuleres.

## Se Også:

For videre læring, se på denne detaljerte veiledningen om parsing av HTML med Rust av Eryk Napierała: https://www.oreilly.com/library/view/rust-programming-by/9781788390637/B073QF4SC2_ch09.xhtml

Eller denne StackOverflow-tråden der community-medlemmer diskuterer fordeler og ulemper med forskjellige Rust HTML-parsing-biblioteker: https://stackoverflow.com/questions/50241738/how-to-parse-html-with-rust