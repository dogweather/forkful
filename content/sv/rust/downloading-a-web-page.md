---
title:                "Hämta en webbsida"
date:                  2024-01-20T17:44:42.308118-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hämta en webbsida"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ladda ner en webbsida innebär att hämta data från internet till din lokal dator. Programmörer gör detta för att bearbeta information, skrapa data, eller testa webbsidor.

## Hur man gör:

```rust
use reqwest; // Lägg till `reqwest` i dina dependencies i Cargo.toml

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    // Adress till webbsidan du vill ladda ner
    let url = "http://example.com";

    // Skicka en GET-förfrågan och vänta på svaret
    let response = reqwest::get(url).await?;

    // Skriv ut rådata som ett strängresultat
    let body = response.text().await?;
    println!("Webbsidans innehåll:\n{}", body);

    Ok(())
}
```
_Utfall: Webbsidans HTML-innehåll visas i terminalen._

## Fördjupning:

Historiskt har webbskrapning och nedladdning av webbsidor varit en utmaning då det krävde hantering av lågnivå nätverksanslutningar och HTTP-protokollet. Nu för tiden finns bibliotek som `reqwest` som förenklar denna process genom att abstrahera komplexa nätverksoperationer.

Det finns andra bibliotek som `curl` och `hyper`, vilka kan lämpa sig bättre för vissa scenarion, men `reqwest` är känt för att vara användarvänligt för de flesta tillämpningar.

I detalj handlar att ladda ner en webbsida om att skicka en HTTP GET-begäran till servern som hostar sidan, ta emot svaret, som ofta är HTML-kod, och sedan använda den datan som behövs. Asynkron kod i Rust med bibliotek som `tokio` är nyckeln för att hantera flera begäranden effektivt.

## Se även:

- Reqwest-dokumentation: https://docs.rs/reqwest/
- Tokio-dokumentation: https://tokio.rs/
- Rust async book: https://rust-lang.github.io/async-book/
- Webb skrapning med Rust: https://rust-lang-nursery.github.io/rust-cookbook/web/scraping.html
