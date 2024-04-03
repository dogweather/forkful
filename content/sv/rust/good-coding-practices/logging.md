---
date: 2024-01-26 01:09:23.979418-07:00
description: "Loggning \xE4r som att f\xF6ra en journal f\xF6r din applikation; det\
  \ \xE4r praxis att registrera h\xE4ndelser, fel och annan relevant data under k\xF6\
  rning. Utvecklare\u2026"
lastmod: '2024-03-13T22:44:37.705010-06:00'
model: gpt-4-1106-preview
summary: "Loggning \xE4r som att f\xF6ra en journal f\xF6r din applikation; det \xE4\
  r praxis att registrera h\xE4ndelser, fel och annan relevant data under k\xF6rning."
title: Loggning
weight: 17
---

## Hur man gör:
Låt oss ställa in ett grundläggande loggningscenario i Rust med hjälp av `log`-craten, som tillhandahåller en loggningsfasad, och `env_logger`, en loggningsimplementering för `log`-craten. Lägg först till dem i din Cargo.toml:

```toml
[dependencies]
log = "0.4.14"
env_logger = "0.9.0"
```

Nu, ställ in och initialisera loggern i din `main.rs`:

```rust
use log::{info, warn};

fn main() {
    env_logger::init();

    info!("Det här är ett informationsmeddelande.");
    warn!("Det här är ett varningsmeddelande.");
}
```

Kör din app med `RUST_LOG=info cargo run`, och du kommer att se utskriften:

```
INFO: Det här är ett informationsmeddelande.
WARN: Det här är ett varningsmeddelande.
```

Lek med `RUST_LOG`-miljövariabeln genom att ställa in den till `error`, `warn`, `info`, `debug` eller `trace` för att kontrollera loggarnas detaljnivå.

## Fördjupning
Konceptet med loggning är inte något nytt; det har funnits sedan datorteknikens tidiga dagar. Innan loggning var vanligt i mjukvara, förlitade sig utvecklare på primitiva metoder som utskriftsuttryck eller felsökningsverktyg för att spåra programexekvering. När programmen blev mer komplexa, ökade också behovet av strukturerade metoder för loggning.

I Rust abstraherar `log`-craten bort detaljerna i loggningsimplementering, vilket låter utvecklare koppla in olika loggningsbakdelar. Även om `env_logger` är ett vanligt val, finns det alternativ som `fern`, `slog` eller `tracing`, var och en med sitt eget utbud av funktioner och konfigurationsalternativ.

Några överväganden när man implementerar loggning inkluderar:

1. **Loggnivåer**: Det är viktigt att kontrollera detaljnivån. Rusts `log`-crate definierar flera loggnivåer: error, warn, info, debug och trace, i avtagande ordning av allvarlighetsgrad.

2. **Prestanda**: Loggning kan påverka prestanda. Det är kritiskt att använda det omdömesgillt, och se till att undvika loggning i prestandakritiska vägar eller överdrivet detaljerade loggar i produktion.

3. **Strukturerad Loggning**: Moderna bästa praxis involverar strukturerad loggning, där loggar skrivs i ett maskinläsbart format som JSON. Bibliotek som `slog` tillåter strukturerad loggning i Rust, som kan indexeras och sökas i logghanteringssystem som ELK Stack eller Splunk.

4. **Asynkron Loggning**: För att minimera påverkan på huvudapplikationen kan loggningen göras asynkront. Detta uppnås ofta genom att loggningsbiblioteket skriver till en minnesburen kö, och en separat tråd bearbetar kön och skriver loggar till destinationen.

5. **Konfiguration**: Många loggningsramverk stödjer konfiguration genom miljövariabler, konfigurationsfiler och/eller kod. Denna flexibilitet är nyckeln till finjustering av utskrift i olika miljöer (utveckling, staging, produktion).

## Se även
- `log`-cratens dokumentation: https://docs.rs/log/
- `env_logger`-cratens dokumentation: https://docs.rs/env_logger/
- Rust by Example-loggningssida: https://doc.rust-lang.org/rust-by-example/std_misc/log.html
- `slog`-craten, ett alternativt loggningsramverk: https://github.com/slog-rs/slog
- Tracing, ett ramverk för instrumentering av Rust-program: https://crates.io/crates/tracing
