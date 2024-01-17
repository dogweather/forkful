---
title:                "Å jobbe med json"
html_title:           "Rust: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/working-with-json.md"
---

{{< edit_this_page >}}

#Hva og Hvorfor?
Når vi jobber med programmering, er det vanlig å komme over data i JSON-format. JSON står for JavaScript Object Notation og er en måte å strukturere og lagre data på. Det er et populært format for å overføre og lagre data, og derfor er det viktig for utviklere å kunne jobbe med det for å kunne håndtere og manipulere data effektivt.

# Hvordan:
Når du jobber med Rust, kan du enkelt håndtere JSON-data ved å bruke biblioteket serde_json. Først trenger du å legge til serde_json i prosjektet ditt ved å legge til følgende kode i din Cargo.toml-fil:

```
[dependencies]
serde_json = "1.0"
```

Deretter kan du begynne å jobbe med JSON-data ved å bruke serde_json::Value-structen. For å legge til data i et nytt JSON-objekt, kan du bruke serde_json::json!-makroen. For eksempel:

```
let data = serde_json::json!({
    "navn": "Ole",
    "alder": 28,
    "hobbyer": ["fotball", "gaming", "musikk"]
});
```

Dette vil generere følgende JSON-objekt:

```
{
    "navn": "Ole",
    "alder": 28,
    "hobbyer": ["fotball", "gaming", "musikk"]
}
```

For å hente data fra et JSON-objekt, kan du bruke serde_json::from_str-funksjonen. For eksempel:

```
let data = r#"{"navn": "Ole", "alder": 28, "hobbyer": ["fotball", "gaming", "musikk"]}"#;
let verdi: serde_json::Value = serde_json::from_str(data).unwrap();
println!("Navn: {}", verdi["navn"]); // Printer navnet "Ole" til konsollen
```

#Dypdykk:
JSON-formatet ble først utviklet i 2001, og har siden blitt populært på grunn av sin enkle syntaks og støtte av de fleste programmeringsspråk. Alternativene til å jobbe med JSON-data i Rust inkluderer serde_cbor som støtter det kompakte CBOR-formatet, og rust-json som gir en mer "rust-style" tilnærming til å jobbe med JSON.

Etter å ha definert et rotnivå for et JSON-dokument, kan serde_json::Value-structen lagde i nestede kloner etter behov uten å definere nye structs. Dette gjør det enkelt å jobbe med komplekse JSON-data uten å måtte skrive mye kodes.

#Se også:
For mer informasjon om å jobbe med JSON-data i Rust, kan du besøke offisiell dokumentasjon for serde_json-biblioteket: https://docs.rs/serde_json/1.0.60/serde_json/

Hvis du er interessert i mer informasjon om JSON-formatet generelt, kan du sjekke ut denne artikkelen på Mozilla Developer Network: https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON