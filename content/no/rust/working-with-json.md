---
title:                "Rust: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en Rust programmerer, har du sannsynligvis hørt om JSON-formatet, men hvorfor bør du engasjere deg i å jobbe med det? Vel, JSON (JavaScript Object Notation) er et populært format for datautveksling, som brukes av mange programmerings- og webutviklingsverktøy. Det er enkelt å lese og skrive, og støttes av de fleste språk og plattformer. Som Rust-utvikler kan du dra nytte av å kunne jobbe med JSON-data og integrere det i prosjektene dine.

## Slik gjør du det

For å jobbe med JSON i Rust, trenger vi først å importere serde og serde_json biblioteker. Disse bibliotekene gjør det enkelt å serialisere og deserialisere JSON-data i Rust. Vi kan installere dem ved å legge følgende avhengighet til Rust-prosjektet vårt: ```serde = { version = "1.0", features = ["derive"] }``` og ``` serde_json = "1.0" ```

La oss nå se på et enkelt eksempel på hvordan vi kan serialisere JSON-data i Rust:

```
#[macro_use]
extern crate serde_derive;

use serde_json::{json, Result};

#[derive(Serialize)]
struct Person {
    name: String,
    age: i32,
    address: String,
}

fn main() -> Result<()> {
    let person = Person {
        name: String::from("John"),
        age: 30,
        address: String::from("123 Main Street"),
    };

    // Serializing the person struct to a JSON string
    let person_json = serde_json::to_string(&person)?;

    println!("{}", person_json); // {"name":"John","age":30,"address":"123 Main Street"}

    Ok(())
}
```

Vi starter med å importere ```json!``` macro som lar oss skrive JSON-data som en Rust verdi. Deretter definerer vi en Person struktur med navn, alder og adresse felt. Merk at vi må legge til ```#[derive(Serialize)]``` makroen over strukturen for å gjøre den serialiserbar. Inne i ```main``` funksjonen oppretter vi en Person-instans og bruker ```serde_json::to_string``` funksjonen for å serialisere den til en JSON-streng. Vi skriver deretter strengen til konsollen og ser output: ```{"name":"John","age":30,"address":"123 Main Street"}```

For å deserialisere JSON-data, kan vi bruke ```serde_json::from_str``` funksjonen. La oss se på et eksempel på det også:

```
use serde_json::{Value, Result};

fn main() -> Result<()> {
    let person_json = r#"{
    "name": "John",
    "age": 30,
    "address": "123 Main Street"
    }"#;

    // Deserializing the JSON string into a Value type
    let person: Value = serde_json::from_str(person_json)?;

    // Accessing the values from the JSON data
    let name = person["name"].as_str().unwrap();
    let age = person["age"].as_i64().unwrap();

    println!("Name: {}, Age: {}", name, age); // Name: John, Age: 30

    Ok(())
}
```

Her legger vi JSON-dataen som en streng i ```person_json``` variabelen. Deretter deserialiserer vi den til en ```Value``` type. Med den kan vi få tilgang til verdiene ved å bruke indeksering og ```as_*``` metodene avhengig av typene. I dette eksemplet får vi tilgang til navnet og alderen til personen og skriver dem til konsollen.

## Utforske JSON videre

Nå har du en grunnleggende forståelse av hvordan du kan jobbe med JSON-data i Rust. Men det er mye mer å utforske og lære om dette emnet. Du kan se på serde og serde_json dokumentasjon for å finne ut mer om tilgjengelige metoder og funksjoner for å jobbe med JSON i Rust. Du kan også sjekke ut prosjekter som serde_path og serde_json_path for å få tilgang til JSON-data ved hjelp av sti-baserte søke- og filtreringsmuligheter.

## Se også

-