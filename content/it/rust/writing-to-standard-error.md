---
title:    "Rust: Scrivere su standard error"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere sullo stream di errore standard può essere utile per segnalare informazioni di debug o errori durante l'esecuzione del codice. Inoltre, può essere utile durante lo sviluppo di programmi in Rust per identificare il punto esatto in cui si verifica un errore.

## Come fare

Per scrivere sullo stream di errore standard in Rust, è necessario utilizzare il modulo `std::io` e il suo metodo `eprintln!()`. In questo modo, è possibile scrivere su stdout un messaggio di errore utilizzando la sintassi di formattazione di Rust:

```Rust
use std::io;

fn main() {
    eprintln!("Questo è un messaggio di errore: {}", "Errore 404");
}
```

Questo codice produrrà l'output `"Questo è un messaggio di errore: Errore 404"` nello stream di errore standard.

Un altro modo per scrivere su stderr in Rust è utilizzare il pacchetto `log`. Con questo approccio, è possibile gestire più tipi di messaggi di log, come `info`, `warn` e `error`. Di seguito un esempio di come utilizzare `log` per scrivere su stderr:

```Rust
use log::{error, info, warn};

fn main() {
    error!("Messaggio di errore");
    info!("Informazioni di debug");
    warn!("Attenzione!");
}
```

L'output di questo codice sarà simile a questo:

```sh
[ERROR] Messaggio di errore
[INFO] Informazioni di debug
[WARN] Attenzione!
```

## Approfondimento

Mentre scrivere su stdout è la soluzione più semplice e veloce, ciò non garantisce che il messaggio sia effettivamente visualizzato per l'utente finale. Invece, scrivere su stderr assicura che il messaggio di errore sia sempre visualizzato, indipendentemente da eventuali flussi di output che potrebbero essere redirezionati.

Inoltre, la scrittura su stderr è più efficiente rispetto a stdout, poiché non richiede alcuna formattazione o buffering del messaggio. Può quindi essere particolarmente utile in contesti ad alta frequenza di errori.

Inoltre, con il pacchetto `log`, si ha la possibilità di configurare le diverse tipologie di messaggi di errore, consentendo una maggiore flessibilità nella gestione dei log.

## Vedi anche

- La documentazione di [std::io](https://doc.rust-lang.org/std/io/) per saperne di più su come scrivere su stderr in maniera efficiente.
- La guida [Logging in Rust with log4rs](https://log4rs.rs/) per una gestione avanzata dei log utilizzando il pacchetto `log`.
- Questo [tutorial su Rust](https://www.rust-lang.org/learn/get-started) per imparare i fondamenti del linguaggio.