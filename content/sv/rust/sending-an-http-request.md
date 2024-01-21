---
title:                "Skicka en http-förfrågan"
date:                  2024-01-20T18:00:53.738891-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en http-förfrågan"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran innebär att din kod begär data från, eller skickar data till, en server på internet. Programmerare gör detta för att interagera med webbtjänster, hämta webbinnehåll eller kommunicera mellan klient och server.

## Hur gör man:
```Rust
// Lägg till biblioteket reqwest i Cargo.toml
// reqwest = "0.11"

use reqwest;
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let url = "http://httpbin.org/get";
    
    // Skicka GET-förfrågan och få ett svar
    let res = reqwest::get(url).await?;
    
    // Skriv ut statuskoden och svaret
    println!("Status: {}", res.status());
    println!("Headers:\n{:#?}", res.headers());
    
    // Omvandla svaret till text
    let body = res.text().await?;
    println!("Body:\n{}", body);

    Ok(())
}
```
Exempel på output:
```
Status: 200 OK
Headers:
{
    "content-type": "application/json",
    ...
}
Body:
{
    "args": {},
    "headers": {
        ...
    },
    ...
}
```

## Fordjupning
Historiskt sett har HTTP-förfrågningar varit själva grunden för webben, möjliggörande kommunikation mellan klienter och servrar sedan tidigt 90-tal. Alternativ till Rusts `reqwest`-bibliotek inkluderar `hyper`, som är en lägre nivås HTTP-implementation, och `surf`, en annan enkel, minimalistisk klient. `reqwest` använder `hyper` under huven och är synkroniserad med `tokio`, vilket ger asynkron I/O och tillåter skalbar och icke-blockerande kommunikation, lämplig i moderna applikationer som kräver hög prestanda.

## Se även
- Reqwest dokumentation: https://docs.rs/reqwest/
- Tokio projektet (asynkron programmering i Rust): https://tokio.rs/
- Rusts officiella dokumentation för asynkron programmering: https://doc.rust-lang.org/book/ch16-00-concurrency.html
- HTTP specifikationen av IETF: https://tools.ietf.org/html/rfc2616