---
title:                "Loggføring"
aliases: - /no/rust/logging.md
date:                  2024-01-26T01:09:07.835118-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggføring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/logging.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Loggføring er som å holde en journal for applikasjonen din; det er praksisen med å registrere hendelser, feil og annen relevant data under kjøretid. Utviklere bruker logger for å diagnostisere problemer, overvåke systemoppførsel, og samle innsikter som driver forbedringer – det er grunnlaget for operasjonell intelligens.

## Hvordan:

La oss sette opp et grunnleggende loggføringsscenario i Rust ved å bruke `log`-kassen, som tilbyr en loggføring facade, og `env_logger`, en loggføringsimplementasjon for `log`-kassen. Først, legg dem til i din Cargo.toml:

```toml
[dependencies]
log = "0.4.14"
env_logger = "0.9.0"
```

Nå, sett opp og initialiser loggeren i din `main.rs`:

```rust
use log::{info, warn};

fn main() {
    env_logger::init();

    info!("Dette er en informasjonsmelding.");
    warn!("Dette er en advarselmelding.");
}
```

Kjør appen din med `RUST_LOG=info cargo run`, og du vil se følgende utskrift:

```
INFO: Dette er en informasjonsmelding.
WARN: Dette er en advarselmelding.
```

Lek rundt med `RUST_LOG` miljøvariabelen ved å sette den til `error`, `warn`, `info`, `debug`, eller `trace` for å kontrollere detaljnivået i loggene dine.

## Dypdykk

Konseptet med loggføring er ikke noe nytt; det har vært rundt siden de tidlige dagene av databehandling. Før loggføring ble vanlig i programvare, stolte utviklere på primitive metoder som utskriftssetninger eller feilsøkingverktøy for å spore programutførelse. Etter hvert som programmene økte i kompleksitet, vokste også behovet for strukturerte tilnærminger til loggføring.

I Rust abstraherer `log`-kassen bort loggføringsdetaljene, noe som lar utviklere plugge inn forskjellige loggbakender. Selv om `env_logger` er et vanlig valg, finnes det alternativer som `fern`, `slog` eller `tracing`, hver med sitt eget sett av funksjoner og konfigurasjonsalternativer.

Noen hensyn når man implementerer loggføring inkluderer:

1. **Logg Nivåer**: Kontroll av detaljnivået er nøkkelen. Rust sin `log`-kasse definerer flere loggnivåer: error, warn, info, debug, og trace, i avtagende rekkefølge av alvorlighetsgrad.

2. **Ytelse**: Loggføring kan påvirke ytelsen. Det er avgjørende å bruke det med omtanke, og sørge for å unngå loggføring i ytelses-kritiske veier eller overdrevet detaljerte logger i produksjon.

3. **Strukturert Loggføring**: Moderne beste praksiser innebærer strukturert loggføring, hvor logger skrives i et maskinlesbart format som JSON. Biblioteker som `slog` tillater for strukturert loggføring i Rust, som kan indekseres og spørres ved hjelp av loggforvaltningssystemer som ELK Stack eller Splunk.

4. **Asynkron Loggføring**: For å minimere påvirkningen på hovedapplikasjonen, kan loggføring gjøres asynkront. Dette oppnås ofte ved å ha loggbiblioteket skrive til en minnebasert kø, og en separat tråd prosesserer køen og skriver logger til bestemmelsesstedet.

5. **Konfigurasjon**: Mange loggframewor ker støtter konfigurasjon gjennom miljøvariabler, konfigurasjonsfiler, og/eller kode. Denne fleksibiliteten er nøkkelen for finjustering av utskriften i forskjellige miljøer (utvikling, staging, produksjon).

## Se Også

- Dokumentasjonen for `log`-kassen: https://docs.rs/log/
- Dokumentasjonen for `env_logger`-kassen: https://docs.rs/env_logger/
- Rust by Example loggeside: https://doc.rust-lang.org/rust-by-example/std_misc/log.html
- `slog`-kassen, et alternativt loggeframewor k: https://github.com/slog-rs/slog
- Tracing, et rammeverk for instrumentering av Rust-programmer: https://crates.io/crates/tracing
