---
date: 2024-01-26 01:08:47.051201-07:00
description: "Il logging \xE8 come tenere un diario per la tua applicazione; \xE8\
  \ la pratica di registrare eventi, errori e altri dati pertinenti durante l'esecuzione.\
  \ Gli\u2026"
lastmod: '2024-02-25T18:49:41.099609-07:00'
model: gpt-4-1106-preview
summary: "Il logging \xE8 come tenere un diario per la tua applicazione; \xE8 la pratica\
  \ di registrare eventi, errori e altri dati pertinenti durante l'esecuzione. Gli\u2026"
title: "Registrazione delle Attivit\xE0 (Logging)"
---

{{< edit_this_page >}}

## Cosa e Perché?

Il logging è come tenere un diario per la tua applicazione; è la pratica di registrare eventi, errori e altri dati pertinenti durante l'esecuzione. Gli sviluppatori utilizzano i log per diagnosticare problemi, monitorare il comportamento del sistema e raccogliere informazioni che guidano il miglioramento: è il pane e il burro dell'intelligenza operativa.

## Come fare:

Impostiamo uno scenario di logging basilare in Rust utilizzando la crate `log`, che fornisce una facciata di logging, e `env_logger`, un'implementazione di logging per la crate `log`. Prima di tutto, aggiungili al tuo Cargo.toml:

```toml
[dependencies]
log = "0.4.14"
env_logger = "0.9.0"
```

Ora, configura e inizializza il logger nel tuo `main.rs`:

```rust
use log::{info, warn};

fn main() {
    env_logger::init();

    info!("Questo è un messaggio informativo.");
    warn!("Questo è un messaggio di avviso.");
}
```

Esegui la tua app con `RUST_LOG=info cargo run`, e vedrai l'output:

```
INFO: Questo è un messaggio informativo.
WARN: Questo è un messaggio di avviso.
```

Gioca con la variabile d'ambiente `RUST_LOG` impostandola su `error`, `warn`, `info`, `debug` o `trace` per controllare la verbosità dei tuoi log.

## Approfondimento

Il concetto di logging non è affatto nuovo; esiste fin dai primi giorni dell'informatica. Prima che il logging diventasse comune nel software, gli sviluppatori si affidavano a metodi primitivi come le istruzioni di stampa o gli strumenti di debug per tracciare l'esecuzione dei programmi. Man mano che i programmi crescevano in complessità, aumentava anche la necessità di approcci strutturati al logging.

In Rust, la crate `log` astrae i dettagli dell'implementazione del logging, permettendo agli sviluppatori di inserire diversi backend di logging. Sebbene `env_logger` sia una scelta comune, esistono alternative come `fern`, `slog` o `tracing`, ognuna con il proprio insieme di funzionalità e opzioni di configurazione.

Alcune considerazioni quando si implementa il logging includono:

1. **Livelli di Log**: Controllare la verbosità è fondamentale. La crate `log` di Rust definisce diversi livelli di log: error, warn, info, debug e trace, in ordine decrescente di gravità.

2. **Prestazioni**: Il logging può influenzare le prestazioni. È critico utilizzarlo con giudizio, assicurandosi di evitare il logging nei percorsi critici per le prestazioni o log eccessivamente dettagliati in produzione.

3. **Logging Strutturato**: Le migliori pratiche moderne coinvolgono il logging strutturato, dove i log sono scritti in un formato leggibile dalle macchine come JSON. Librerie come `slog` permettono il logging strutturato in Rust, che può essere indicizzato e interrogato utilizzando sistemi di gestione dei log come ELK Stack o Splunk.

4. **Logging Asincrono**: Per ridurre al minimo l'impatto sull'applicazione principale, il logging può essere eseguito in modo asincrono. Questo è spesso realizzato facendo scrivere la libreria di logging su una coda in memoria, e un thread separato elabora la coda e scrive i log sulla destinazione.

5. **Configurazione**: Molti framework di logging supportano la configurazione attraverso variabili d'ambiente, file di configurazione e/o codice. Questa flessibilità è fondamentale per regolare l'output in diversi ambienti (sviluppo, staging, produzione).

## Vedi Anche

- La documentazione della crate `log`: https://docs.rs/log/
- La documentazione della crate `env_logger`: https://docs.rs/env_logger/
- La pagina di logging di Rust by Example: https://doc.rust-lang.org/rust-by-example/std_misc/log.html
- La crate `slog`, un framework di logging alternativo: https://github.com/slog-rs/slog
- Tracing, un framework per strumentare programmi Rust: https://crates.io/crates/tracing
