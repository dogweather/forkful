---
title:                "Arbeid med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Jobbing med JSON dreier seg om å håndtere data i JavaScript Object Notation format, som er lett for mennesker å lese og skrive, samt lett for maskiner å tolke og generere. Programmerere bruker JSON til å utveksle data mellom server og webapplikasjoner, eller for å lagre konfigurasjoner og innstillinger.

## Hvordan Gjøre Det:
```Rust
use serde::{Deserialize, Serialize};
use serde_json::{Result, Value};

// Definer en struktur som representerer dataen vi ønsker å arbeide med
#[derive(Serialize, Deserialize, Debug)]
struct Person {
    navn: String,
    alder: u8,
    epost: String,
}

fn main() -> Result<()> {
    // JSON streng
    let data = r#"
        {
            "navn": "Ola Nordmann",
            "alder": 30,
            "epost": "ola@norge.no"
        }"#;

    // Deserialize JSON til en Rust struktur
    let p: Person = serde_json::from_str(data)?;

    println!("{:?}", p);

    // Endre data og serialize tilbake til JSON
    let endret_person = Person {
        navn: p.navn,
        alder: p.alder + 1,
        epost: p.epost,
    };

    let j = serde_json::to_string(&endret_person)?;
    println!("{}", j);

    Ok(())
}
```
Utdata:
```plaintext
Person { navn: "Ola Nordmann", alder: 30, epost: "ola@norge.no" }
{"navn":"Ola Nordmann","alder":31,"epost":"ola@norge.no"}
```

## Dypdykk
JSON ble offisielt standardisert i 2013, men har røtter til JavaScript og var allerede populær lang tid før. I Rust brukes `serde` biblioteket til serialisering/deserialisering. Alternativer inkluderer XML, men JSON er mer konsist og kodet i UTF-8. Implementasjonsdetaljer viktig for Rust-utviklere inkluderer korrekt bruk av traits som `Serialize` og `Deserialize` og god feilhåndtering ved bruk av `Result`.

## Se Også
- Serde offisielle dokumentasjon: https://serde.rs/
- JSON standard spesifikasjon: https://www.json.org/json-no.html
- En introduksjon til Rust: https://www.rust-lang.org/learn
