---
title:                "Rust: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

JSON (JavaScript Object Notation) är ett vanligt filformat som används för att lagra och utbyta data mellan applikationer. Att kunna arbeta med JSON i Rust ger dig som programmerare möjlighet att behandla och manipulera data på ett effektivt och säkert sätt, tack vare Rusts starka typsystem och minnessäkerhet. 

## Hur man gör

För att kunna jobba med JSON i Rust behöver du inkludera biblioteket `serde_json` i ditt projekt genom att lägga till följande rad i din `Cargo.toml` fil:

```
serde_json = "1.0"
```

Sedan kan du enkelt läsa in en JSON-fil genom att använda funktionen `from_reader` och sedan anropa dess `unwrap`-metod för att extrahera datan som en `Value`-typ som kan anta olika värden baserat på JSON-datastrukturen.

```
use serde_json::Value;

fn main() { 
    // Skapar en tidigare reducerad JSON-fil
    let data = r#"{"name":"Kalle", "age":26, "interests":["fotboll", "programmering"]}"#;
    
    // Med hjälp av "from_reader" funktionen och "unwrap" metoden kan vi extrahera datan.
    let json: Value = serde_json::from_reader(data.as_bytes()).unwrap(); 
    
    // Vi kan sedan arbeta med datan som en "Value" typ och till exempel printa ut värdet för "name".
    println!("Namn: {}", json["name"]);
    
    // Samma princip gäller för att skapa och skriva till en JSON-fil.
    // Först skapar vi ett nytt "Value" objekt med hjälp av "serde_json::Value::Object" funktionen.
    let mut new_json = serde_json::Value::Object(data.clone());
    
    // Sedan kan vi lägga till ett nytt värde genom att anropa "insert" funktionen,
    // där första argumentet är nyckeln och andra argumentet är värdet.
    new_json["country"] = serde_json::Value::String("Sverige".to_string()); 
    
    // Slutligen skriver vi till vår nya JSON-fil.
    let mut file = std::fs::File::create("output.json").expect("Kunde inte skapa fil!");
    serde_json::to_writer_pretty(&mut file, &new_json).expect("Kunde inte konvertera till JSON"); 
}
```

Output:

```
Namn: Kalle
```

Resultatet blir en JSON-fil som heter `output.json` med följande struktur:

```
{
  "name": "Kalle",
  "age": 26,
  "interests": [
    "fotboll",
    "programmering"
  ],
  "country": "Sverige"
}
```

## Djupdykning

Genom att arbeta med `serde_json` biblioteket får du tillgång till många praktiska funktioner för att behandla JSON-data. Du kan till exempel:

- Konvertera en komplex datastruktur till JSON-format med funktionen `serde_json::to_string` eller `serde_json::to_writer`.

- Kontrollera och manipulera JSON-datans struktur med hjälp av `serde_json::Document` typen.

- Läsa och skriva JSON-data från och till filer med `serde_json::from_str` och `serde_json::from_reader` funktionerna.

Genom att lära dig använda dessa funktioner kan du effektivt hantera JSON-filer i dina Rust-projekt.

## Se även

- [Officiell serde_json dokumentation](https://docs.serde.rs/serde_json/)

- [Tutorial: Effektiv datahantering med serde i Rust](https://blog.logrocket.com/data-handling-in-rust-with-serde/)