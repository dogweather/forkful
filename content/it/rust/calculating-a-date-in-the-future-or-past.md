---
title:    "Rust: Calcolare una data nel futuro o nel passato"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Perché

Se ti sei mai chiesto come calcolare una data nel futuro o nel passato utilizzando il linguaggio di programmazione Rust, allora questo articolo è quello che fa per te. Imparerai come utilizzare le librerie standard di Rust per gestire le date e i tempi in modo semplice ed efficiente.

## Come fare 

Per calcolare una data in futuro o passato in Rust, è necessario utilizzare il modulo `chrono` della libreria standard. Innanzitutto, sarà necessario importare il modulo utilizzando la seguente dichiarazione:

```Rust
use chrono::prelude::*;
```

Successivamente, possiamo creare un oggetto `DateTime` che rappresenta una data specifica, utilizzando il metodo `new` e specificando l'anno, il mese, il giorno, l'ora, i minuti e i secondi. Ad esempio, se volessimo rappresentare il 1 gennaio 2022 alle ore 10:30:00, useremmo il seguente codice:

```Rust
let date = Utc.ymd(2022, 1, 1).and_hms(10, 30, 0);
```

Per calcolare una data nel futuro o nel passato, possiamo utilizzare il metodo `with_duration_since` passando come parametro un oggetto `Duration` che rappresenta la differenza di tempo desiderata. Ad esempio, se volessimo ottenere la data 10 giorni dopo la data creata in precedenza, useremmo il seguente codice:

```Rust
let new_date = date.with_duration_since(Duration::days(10));
```

Infine, per ottenere il valore effettivo della data in un formato leggibile, possiamo utilizzare il metodo `to_rfc2822` come mostrato di seguito:

```Rust
println!("La nuova data è: {}", new_date.to_rfc2822());
```

Il codice completo del nostro esempio sarebbe il seguente:

```Rust
use chrono::prelude::*;

fn main() {
    let date = Utc.ymd(2022, 1, 1).and_hms(10, 30, 0);
    let new_date = date.with_duration_since(Duration::days(10));
    println!("La nuova data è: {}", new_date.to_rfc2822());
}
```

E l'output sarebbe:

```
La nuova data è: Tue, 11 Jan 2022 10:30:00 +0000
```

## Approfondimento

La libreria `chrono` di Rust offre una vasta gamma di funzionalità per gestire le date e i tempi in modo preciso e affidabile. Ad esempio, è possibile utilizzare il metodo `with_timezone` per impostare il fuso orario della data, il metodo `with_timezone_offset` per specificare un offset rispetto al fuso orario UTC e molto altro ancora. Per ulteriori dettagli e informazioni sulla libreria, si consiglia di consultare la documentazione ufficiale.

## Vedi anche

- [Documentazione ufficiale di `chrono`](https://docs.rs/chrono/)
- [Tutorial su come gestire le date in Rust](https://www.codementor.io/@arpitbhayani/handling-dates-and-time-in-rust-o5m4pbgmt)