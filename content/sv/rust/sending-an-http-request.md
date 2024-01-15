---
title:                "Sända en http-begäran"
html_title:           "Rust: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Du kanske undrar varför man skulle vilja skicka en HTTP-förfrågan från ett program skrivet i Rust. Det finns många möjliga anledningar, till exempel för att hämta data från en webbserver, kommunicera med ett API, eller ladda ner filer från internet.

## Hur man gör

För att skicka en HTTP-förfrågan i Rust behöver du använda ett bibliotek som hanterar HTTP-kommunikation. Ett populärt exempel är reqwest, som du kan installera genom att lägga till följande rad i din projektets Cargo.toml fil:

```Rust
[dependencies]
reqwest = { version = "0.11.3", features = ["blocking", "json"] }
```

Nästa steg är att importera biblioteket i ditt program:

```Rust
use reqwest::blocking::Client;
```

Nu kan du använda Client-objektet för att skicka en HTTP-förfrågan. Här är ett exempel på hur man gör en GET-förfrågan till Google.com och skriver ut svaret:

```Rust
let client = reqwest::blocking::Client::new();
let response = client.get("http://google.com").send().unwrap();
println!("Statuskod: {}", response.status());
println!("Huvudinnehåll:\n{}", response.text().unwrap());
```

Det här är bara en enkel förfrågan, men du kan också skicka mer komplexa förfrågningar med till exempel anpassade HTTP-headrar eller POST-data.

## Djupdykning

När du skickar en HTTP-förfrågan finns det många delar som sker bakom kulisserna. Först och främst måste du etablera en TCP-anslutning till servern som du vill kommunicera med. Sedan måste du skicka förfrågan i en korrekt formaterad HTTP-överföring. När servern svarar måste du läsa och tolka svaret och hantera eventuella fel.

Det här är en förenklad beskrivning, men det är viktigt att förstå att HTTP-kommunikation inte är en trivial process. Det är därför vi använder oss av bibliotek som reqwest, som hanterar alla dessa steg åt oss.

## Se även

- [officiell dokumentation för reqwest](https://docs.rs/reqwest/0.11.3/reqwest/)
- [introduktion till HTTP i Rust](https://rust-lang-nursery.github.io/rust-cookbook/web/clients.html)
- [tutorial om hur man skapar ett enkelt HTTP-API i Rust](https://blog.logrocket.com/creating-an-http-api-in-rust/)