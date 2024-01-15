---
title:                "Arbeta med json"
html_title:           "Rust: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

Varför skulle man vilja arbeta med JSON i Rust? JSON (JavaScript Object Notation) är ett populärt format för datautbyte och används ofta i webbutveckling och API:er. Genom att lägga till stöd för JSON i dina Rust-projekt kan du enkelt implementera möjligheten att både läsa och skriva data i detta format, vilket kan vara användbart för många olika applikationer.

## Så här gör du

För att arbeta med JSON i Rust behöver du först importera "serde" biblioteket och dess "serde_json" modul genom att lägga till följande kod i din "Cargo.toml" fil:

```rust
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

Därefter kan du importera serde och serde_json i din Rust-kod med hjälp av följande kod:

```rust
extern crate serde;
extern crate serde_json;

use serde::{Deserialize, Serialize};
```

Nu är det dags att skapa en struktur i din kod för att representera den data du vill läsa eller skriva. Du kan göra detta genom att använda "derive" makrona och ange #[derive (Serialize, Deserialize)] ovanför din struktur enligt följande:

```rust
#[derive(Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8, 
    address: String,
}
```

För att läsa data från en JSON-fil och konvertera den till vår Person-struktur, kan vi använda serde_json biblioteket och dess "from_reader" funktion. Låt oss anta att vi har en fil som heter "person.json" med följande innehåll:

```json
{
    "name": "Anna",
    "age": 25,
    "address": "Göteborg"
}
```

Vi kan läsa innehållet i denna fil och konvertera den till vår Person-struktur med hjälp av följande kod:

```rust
let file = File::open("person.json").expect("File not found.");
let reader = BufReader::new(file);
let person: Person = serde_json::from_reader(reader).expect("Failed to read from file.");
```

På samma sätt kan vi även skriva data till en JSON-fil från vår Person-struktur med hjälp av serde_json och dess "to_writer" funktion:

```rust
let file = File::create("person.json").expect("Failed to create file.");
let writer = BufWriter::new(file);
serde_json::to_writer(writer, &person).expect("Failed to write to file.");
```

## Djupdykning

Det finns många olika möjligheter när det kommer till att arbeta med JSON i Rust. Ett annat användbart verktyg är "json!" makrona som finns i serde_json biblioteket. Den gör det möjligt att skapa en JSON-sträng direkt från din kod utan att behöva skapa en struktur först. Exempel:

```rust
let person_json = json!({
    "name": "Erik",
    "age": 30,
    "address": "Stockholm"
});
```

Du kan också använda serde_json för att konvertera en JSON-sträng till ett dynamiskt objekt med hjälp av "json::from_str" funktionen. Detta kan vara användbart om du vill läsa en okänd JSON-sträng och accessa dess värden dynamiskt.

```rust
let json_string = r#"{"name": "Maria", "age": 22, "address": "Malmö"}"#;
let dynamic_object = serde_json::from_str(json_string).expect("Failed to parse JSON string.");
println!("Name: {}", dynamic_object["name"]); //output: Name: Maria
```

## Se även

- [Serde documentation](https://docs.rs/serde/)
- [Serde JSON documentation](https://docs.rs/serde_json/)
- [Rust JSON crate list](https://rust-lang-nursery.github.io/rust-cookbook/web/clients/json.html)