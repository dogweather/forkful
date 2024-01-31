---
title:                "Arbeta med JSON"
date:                  2024-01-19
simple_title:         "Arbeta med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON, eller JavaScript Object Notation, är ett textbaserat dataformat som används för att lagra och utbyta data. Programmerare använder JSON för att hantera data mellan olika system på ett enkelt och lättläst sätt.

## Hur gör man:
Rust använder `serde`-krate för att serialisera och deserialisera JSON-data. Nedan en exempelkod:

```rust
use serde::{Deserialize, Serialize};
use serde_json::Result;

// Define en struktur som motsvarar din data
#[derive(Serialize, Deserialize)]
struct Användare {
    namn: String,
    ålder: u8,
    epost: String,
}

fn main() -> Result<()> {
    // Skapa ett användarobjekt
    let användare = Användare {
        namn: "Anna Svensson".to_string(),
        ålder: 30,
        epost: "anna.svensson@example.com".to_string(),
    };

    // Serialisera det till en JSON-sträng
    let serialiserad = serde_json::to_string(&användare)?;
    println!("Serialiserad: {}", serialiserad);

    // Deserialisera strängen tillbaka till en Användare
    let deserialiserad: Användare = serde_json::from_str(&serialiserad)?;
    println!("Deserialiserad: {} {}", deserialiserad.namn, deserialiserad.ålder);

    Ok(())
}
```
Resultatet blir:
```
Serialiserad: {"namn":"Anna Svensson","ålder":30,"epost":"anna.svensson@example.com"}
Deserialiserad: Anna Svensson 30
```

## Djupdykning
JSON skapades i början av 2000-talet och blev en del av ECMAScript-standard 2013. Alternativ till JSON är till exempel XML och YAML. JSON används ofta för webbaserade API:er tack vare sin kompakthet och läsbarhet. Rust hanterar JSON effektivt genom `serde`-krate, som erbjuder kraftfull serialisering och deserialisering med minimal prestandapåverkan.

## Se även
- Serde officiella dokumentation: [https://serde.rs/](https://serde.rs/)
- Serde JSON krate dokumentation: [https://docs.serde.rs/serde_json/](https://docs.serde.rs/serde_json/)
- Mer om JSON-formatet: [https://www.json.org/json-sv.html](https://www.json.org/json-sv.html)
