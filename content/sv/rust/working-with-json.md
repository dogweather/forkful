---
title:                "Arbeta med JSON"
html_title:           "Rust: Arbeta med JSON"
simple_title:         "Arbeta med JSON"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Working with JSON (JavaScript Object Notation) is a way for programmers to store and exchange data in a readable and structured format. It's commonly used for communication between different programs and systems, making it an essential skill for any programmer.

## Så här:
Vi kan använda Rust's inbyggda Bibliotek serde för att enkelt hantera JSON-data. Här är ett exempel på hur vi kan läsa och skriva JSON-data, samt skriva ut det till konsolen:

```Rust
use serde_json::{Value, Result};

fn main() -> Result<()> {
    // Läs data från fil
    let data = "{\"name\": \"Emma\", \"age\": 25, \"hobby\": \"programming\"}";

    // Konvertera till JSON-objekt
    let json: Value = serde_json:: from_str(data)?;

    // Skriv ut JSON-data
    println!("{} is {} years old and her hobby is {}", json["name"], json["age"], json["hobby"]);

    // Konvertera tillbaka till sträng och skriv till fil
    let data_written = serde_json::to_string(&json)?;
    println!("{}", data_written);
    Ok(())
}
```

Output:
```
Emma is 25 years old and her hobby is programming
{"name":"Emma","age":25,"hobby":"programming"}
```

## Djupdykning:
JSON har funnits sedan 2001 och är en populär ersättning för XML-formatet. Det är lättare att läsa och skriva än XML, men saknar vissa av dess funktioner, som möjligheten att validera data mot ett schema. Andra alternativ till JSON är CSV (Comma Separated Values) och YAML (YAML Ain't Markup Language). Men JSON är fortfarande det mest använda formatet för datautbyte.

I serde-biblioteket används en datastruktur som heter "Value" för att representera JSON-data, vilket kan vara en sträng, ett nummer, en bool eller en annan Value. Detta tillåter flexibilitet när man läser eller skriver JSON-data, men också kräver lite extra kod för att hämta specifika värden.

## Se också:
- [serde_json Dokumentation](https://docs.serde.rs/serde_json/) - Officiell dokumentation för serde-biblioteket för JSON-hantering i Rust.
- [Rust Cookbook - Läsa och Skriva JSON](https://rust-lang-nursery.github.io/rust-cookbook/web/encoding/working_with_json.html) - Ett brett utbud av kodexempel för att hantera JSON i Rust.
- [JSON Formatter & Validator](https://jsonformatter.org/) - En användbar online-verktyg för att formatera och validera JSON-data.