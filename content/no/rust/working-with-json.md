---
title:                "Å arbeide med json"
html_title:           "Rust: Å arbeide med json"
simple_title:         "Å arbeide med json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du driver med datahåndtering eller webutvikling, er sannsynligheten stor for at du har vært i kontakt med JSON-formatet (JavaScript Object Notation). Det er et populært format for å utveksle data mellom ulike programmer og systemer. Derfor kan det være nyttig å kunne håndtere dette formatet i Rust-programmeringsspråket.

## Hvordan

Det finnes flere måter å jobbe med JSON i Rust. Den enkleste måten er å bruke et bibliotek som allerede eksisterer, som for eksempel serde_json. Dette biblioteket gjør det enkelt å konvertere mellom JSON og Rust sine egne datastrukturer, ved hjelp av annoteringer i koden.

```Rust
extern crate serde;
extern crate serde_json;

use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
    city: String,
}

fn main() {
    // Oppretter et person-objekt
    let person = Person {
        name: String::from("Ola"),
        age: 25,
        city: String::from("Oslo"),
    };

    // Konverterer person-objektet til JSON
    let json = serde_json::to_string(&person).unwrap();

    // Skriver ut JSON-en til konsollen
    println!("{}", json);

    // Konverterer JSON tilbake til person-objektet
    let deserialized_person: Person = serde_json::from_str(&json).unwrap();

    // Skriver ut informasjon om personen
    println!("Navn: {}", deserialized_person.name);
    println!("Alder: {}", deserialized_person.age);
    println!("By: {}", deserialized_person.city);
}
```

Eksempel på utskrift:

```
{"name":"Ola","age":25,"city":"Oslo"}
Navn: Ola
Alder: 25
By: Oslo
```

## Dypdykk

Å arbeide med JSON kan være mer komplisert enn bare å konvertere til og fra datastrukturer i Rust. En vanlig problemstilling er å jobbe med store mengder data, for eksempel når man utveksler data mellom ulike API-er. I slike tilfeller kan det være nyttig å bruke en streaming parser, som jsonstream eller rust-jsonstream. Dette gjør det mulig å lese JSON-data bit for bit, uten å måtte lagre hele strukturen i minnet på en gang.

Et annet aspekt å vurdere når man arbeider med JSON er feilhåndtering. Noen ganger kan det være nyttig å kunne gi mer beskrivende feilmeldinger når noe går galt under konvertering eller parsing av JSON-data. Dette kan oppnås ved å bruke serde_jsons "pretty" funksjonalitet, som gir en mer forståelig utskrift av eventuelle feil.

## Se også

- [serde-json](https://github.com/serde-rs/json) - det mest brukte biblioteket for å håndtere JSON i Rust.
- [jsonstream](https://github.com/cdumay/jsonstream) - en streaming parser for JSON i Rust.
- [rust-jsonstream](https://github.com/dastrobu/rust-jsonstream) - en annen streaming parser for JSON i Rust.