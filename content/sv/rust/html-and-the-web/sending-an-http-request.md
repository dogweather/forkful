---
date: 2024-01-20 18:00:53.738891-07:00
description: "Hur g\xF6r man: Exempel p\xE5 output."
lastmod: '2024-04-05T21:53:39.023268-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Skicka en http-f\xF6rfr\xE5gan"
weight: 44
---

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
