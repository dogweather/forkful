---
title:                "Analyse av HTML"
date:                  2024-01-20T15:33:40.383950-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML betyr å omforme HTML-koden til en struktur en Rust-app kan jobbe med. Vi gjør dette for å kunne dra ut data, skifte innhold eller for å gjøre web scraping.

## How to:
For å parse HTML i Rust kan vi bruke `scraper`-biblioteket. Installasjon avhenger av å legge det til i `Cargo.toml`. Her er et enkelt eksempel:

```rust
use scraper::{Html, Selector};

fn main() {
    let html_str = r#"<p>Hei Verden!</p>"#;
    let document = Html::parse_document(html_str);
    let selector = Selector::parse("p").unwrap();
    
    for element in document.select(&selector) {
        println!("{}", element.inner_html());
    }
}
```

Kjør koden, og du får følgende utskrift:
```
Hei Verden!
```

## Deep Dive
Parsing av HTML er ikke nytt. Det har vært sentralt i mange oppgaver siden webens barndom. Alternativer til Rust inkluderer BeautifulSoup i Python, Nokogiri i Ruby, eller jsoup i Java.

I Rust er `scraper` effektiv fordi den bygger på `html5ever` og `selectors` pakker, kjent for sin hastighet og standard-samsvar. 'html5ever', utviklet av Servo prosjektet, er spesielt konstruert for moderne webstandarder.

Bruk av `scraper` kan involvere å lage selektorer for spesifikke HTML elementer, hantering av klasser, IDer, eller endre innhold dynamisk. Det er også mulig å håndtere mer avanserte oppgaver som å traversere DOM-treet eller å filtrere ut bestemte noder.

## See Also
- Rust `scraper` dokumentasjon: https://docs.rs/scraper/
- Servo prosjektet: https://servo.org/
- `html5ever`: https://github.com/servo/html5ever
- W3C HTML spesifikasjoner: https://www.w3.org/TR/html/
