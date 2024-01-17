---
title:                "Laste ned en nettside"
html_title:           "Rust: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Å laste ned en nettside betyr å få tilgang til og lagre alle informasjonene og ressursene som finnes på siden på din egen datamaskin. Dette gjøres for å kunne behandle og manipulere informasjonen på en mer effektiv måte, for eksempel å vise den på en annen måte eller analysere den.

Hvordan:

For å laste ned en nettside i Rust, kan du bruke curl biblioteket som gir et enkelt grensesnitt for å gjøre HTTP-forespørsler. Her er et eksempel som laster ned nettsiden til Rust-språket og skriver ut innholdet i konsollen: 

```Rust
extern crate curl;

use std::io::{stdout, Write};
use curl::easy::Easy;

fn main() {
    let mut handle = Easy::new();
    handle.url("https://www.rust-lang.org").unwrap();
    handle.write_function(|data| {
        Ok(stdout().write(data).unwrap())
    }).unwrap();
    handle.perform().unwrap();
}
```

Dette vil skrive ut hele HTML-koden til nettsiden. Du kan også bruke curl for å laste ned innholdet til en bestemt URL og lagre det i en fil. 

Dypdykk:

Laste ned en nettside har vært en viktig del av utviklerverktøy i lang tid. Tidligere var det vanlig å bruke biblioteker som wget eller cURL fra kommandolinjen for å laste ned nettsider, men nå kan man enkelt gjøre det direkte i koden. Alternativene for å laste ned en nettside i Rust er begrensede, men man kan også bruke biblioteker som reqwest eller hyper.

Se også:

- Offisiell dokumentasjon for Rust curl: https://docs.rs/curl
- Github-siden til curl biblioteket: https://github.com/alexcrichton/curl-rust