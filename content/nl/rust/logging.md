---
title:                "Logboekregistratie"
aliases:
- nl/rust/logging.md
date:                  2024-01-28T22:03:29.151930-07:00
model:                 gpt-4-0125-preview
simple_title:         "Logboekregistratie"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/logging.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Loggen is als het bijhouden van een dagboek voor je applicatie; het is de praktijk van het vastleggen van gebeurtenissen, fouten en andere relevante gegevens tijdens runtime. Ontwikkelaars gebruiken logs om problemen te diagnosticeren, het gedrag van systemen te monitoren en inzichten te verzamelen die verbeteringen stimuleren - het is het brood en boter van operationele intelligentie.

## Hoe te:

Laten we een basis logscenario in Rust opzetten met behulp van de `log` crate, die een logging facade biedt, en `env_logger`, een logimplementatie voor de `log` crate. Voeg ze eerst toe aan je Cargo.toml:

```toml
[dependencies]
log = "0.4.14"
env_logger = "0.9.0"
```

Stel nu de logger in en initialiseer deze in je `main.rs`:

```rust
use log::{info, warn};

fn main() {
    env_logger::init();

    info!("Dit is een informatiebericht.");
    warn!("Dit is een waarschuwingsbericht.");
}
```

Draai je app met `RUST_LOG=info cargo run`, en je zult de output zien:

```
INFO: Dit is een informatiebericht.
WARN: Dit is een waarschuwingsbericht.
```

Speel met de omgevingsvariabele `RUST_LOG` door het in te stellen op `error`, `warn`, `info`, `debug`, of `trace` om de gedetailleerdheid van je logs te regelen.

## Diepere Duik

Het concept van loggen is niets nieuws; het bestaat al sinds de begindagen van de informatica. Voordat loggen gebruikelijk was in software, vertrouwden ontwikkelaars op primitieve methoden zoals printopdrachten of debugger-tools om de uitvoering van programma's te traceren. Naarmate programma's complexer werden, groeide ook de behoefte aan gestructureerde benaderingen van loggen.

In Rust abstraheert de `log` crate de implementatiedetails van loggen, waardoor ontwikkelaars verschillende logbackends kunnen inpluggen. Hoewel `env_logger` een gangbare keuze is, zijn er alternatieven zoals `fern`, `slog`, of `tracing`, elk met hun eigen reeks functies en configuratieopties.

Enkele overwegingen bij het implementeren van loggen omvatten:

1. **Logniveaus**: Het beheren van de detaillering is essentieel. Rust's `log` crate definieert verschillende logniveaus: error, warn, info, debug en trace, in aflopende volgorde van ernst.

2. **Prestatie**: Loggen kan de prestatie beïnvloeden. Het is cruciaal om het verstandig te gebruiken, ervoor te zorgen dat je niet logt in prestatie-kritieke paden of overmatig gedetailleerde logs in productie hebt.

3. **Gestructureerd Loggen**: Moderne beste praktijken omvatten gestructureerd loggen, waarbij logs worden geschreven in een machineleesbaar formaat zoals JSON. Libraries zoals `slog` maken gestructureerd loggen in Rust mogelijk, dat geïndexeerd en opgevraagd kan worden met behulp van logbeheersystemen zoals ELK Stack of Splunk.

4. **Asynchroon Loggen**: Om de impact op de hoofdapplicatie te minimaliseren, kan loggen asynchroon worden uitgevoerd. Dit wordt vaak bereikt door de logbibliotheek te laten schrijven naar een in-memory wachtrij, en een aparte thread verwerkt de wachtrij en schrijft logs naar de bestemming.

5. **Configuratie**: Veel logframeworks ondersteunen configuratie via omgevingsvariabelen, configuratiebestanden en/of code. Deze flexibiliteit is essentieel voor het fijn afstemmen van de output in verschillende omgevingen (ontwikkeling, staging, productie).

## Zie Ook

- De documentatie van de `log` crate: https://docs.rs/log/
- De documentatie van de `env_logger` crate: https://docs.rs/env_logger/
- Rust by Example logpagina: https://doc.rust-lang.org/rust-by-example/std_misc/log.html
- De `slog` crate, een alternatief logframework: https://github.com/slog-rs/slog
- Tracing, een framework voor het instrumenteren van Rust-programma's: https://crates.io/crates/tracing
