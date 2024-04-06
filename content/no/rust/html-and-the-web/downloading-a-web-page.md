---
date: 2024-01-20 17:44:50.217750-07:00
description: "How to: (Hvordan:) Ved kj\xF8ring vil dette skrive ut HTML-innholdet\
  \ til `http://example.com`."
lastmod: '2024-04-05T21:53:41.551391-06:00'
model: gpt-4-1106-preview
summary: "(Hvordan:) Ved kj\xF8ring vil dette skrive ut HTML-innholdet til `http://example.com`."
title: Nedlasting av en nettside
weight: 42
---

## How to: (Hvordan:)
```Rust
use reqwest;
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let url = "http://example.com";
    let response = reqwest::get(url).await?;

    let body = response.text().await?;
    println!("Nettsideinnhold: {}", body);
    Ok(())
}
```
Ved kjøring vil dette skrive ut HTML-innholdet til `http://example.com`.

## Deep Dive (Dykk dypere)
I de gode gamle dager, brukte vi `curl` eller `wget` fra kommandolinjen, eller HTTP-biblioteker som `libcurl` for programmering. I Rust-verdenen, er `reqwest`-biblioteket nå populært for å håndtere HTTP-forespørsler, takket være sin asynkrone støtte og enkel bruk.

Alternativene inkluderer `hyper` for lavnivå HTTP-operasjoner og `surf` som et annet høynivåalternativ. `reqwest` er imidlertid favoritten for mange fordi det abstraherer bort mange av de komplekse detaljene ved HTTP-forespørsler.

For å virkelig forstå hvordan nedlasting av nettsider fungerer, bør man ha en forståelse av HTTP-protokollen, TCP/IP og DNS-oppslag.

## See Also (Se også)
- [reqwest crate dokumentasjon](https://docs.rs/reqwest/)
- [Rust’s asynkrone programmeringsguide](https://rust-lang.github.io/async-book/)
- [HTTP Made Really Easy](http://www.jmarshall.com/easy/http/) - En guide for å forstå HTTP.
