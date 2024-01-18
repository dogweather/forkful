---
title:                "Å analysere en dato fra en streng."
html_title:           "Rust: Å analysere en dato fra en streng."
simple_title:         "Å analysere en dato fra en streng."
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?
Å parse en dato fra en streng betyr å konvertere en tekstrepresentasjon av en dato til et datatypenotat som kan bli behandlet av datamaskinen. Dette er viktig fordi mange programmer trenger å håndtere datoer, og å ha en måte å konvertere tekstformatet til et datatypenotat gjør det enklere å behandle og manipulere datoer.

Slik Gjør Du:
```Rust
use chrono::prelude::*;

fn main() {
    // Opprett en strengrepresentasjon av en dato
    let dato = "5 Juli 2021";

    // Parse datoen til et datatypenotat
    let parsed_dato = NaiveDate::parse_from_str(dato, "%e %B %Y").unwrap();

    println!("{:?}", parsed_dato); // Output: "2021-07-05"
}
```

Deep Dive:
Å parse en dato fra en streng er en vanlig utfordring for programmerere, spesielt når man tar hensyn til forskjellige tekstrepresentasjoner av datoer som varierer fra land til land. Heldigvis har Rust et utmerket bibliotek kalt "chrono" som gjør det enkelt å håndtere datoer og konvertere dem fra en form til en annen. Alternativer til å bruke "chrono" inkluderer JavaScript-biblioteket "Moment.js" og Python's "datetime" modul.

Se Også:
- Chrono Dokumentasjon: https://docs.rs/chrono/latest/chrono/index.html
- Moment.js: https://momentjs.com/
- Python Datetime Modul: https://docs.python.org/3/library/datetime.html