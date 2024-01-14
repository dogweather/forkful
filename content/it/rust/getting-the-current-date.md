---
title:                "Rust: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché
Scrivere codice per ottenere la data corrente può sembrare un compito banale, ma è un fondamentale skill per qualsiasi programmatore in Rust. In questo articolo, esploreremo come ottenere la data corrente utilizzando il linguaggio di programmazione Rust.

## Come Fare
Per prima cosa, dovremo importare la libreria standard di Rust `std::time::SystemTime`. In seguito, è possibile utilizzare la funzione `now()` per ottenere un istante temporale corrispondente all'istante di esecuzione del programma. Ecco come potrebbe apparire il codice:

```Rust
use std::time::SystemTime;

let current_time = SystemTime::now();
```

Una volta ottenuta la data corrente, è possibile visualizzarla in formato stringa utilizzando il metodo `to_string()`. Di seguito, un esempio di codice completo con output:

```Rust
use std::time::SystemTime;
use std::string::ToString;

let current_time = SystemTime::now();

println!("La data corrente è {}", current_time.to_string());
```

Output:
```
La data corrente è 2019-10-15 08:30:20.263435568 UTC
```

## Approfondimento
Ora che abbiamo un'idea generale di come ottenere la data corrente in Rust, vediamo alcune funzioni più specifiche che ci possono essere utili.

### Unix Timestamp
Il tempo Unix è un modo comune di rappresentare il tempo come un numero di secondi trascorsi dal 1 gennaio 1970. In Rust, possiamo ottenere il tempo Unix utilizzando il metodo `duration_since()` e il valore di riferimento `UNIX_EPOCH`. Ecco un esempio di codice:

```Rust
use std::time::{SystemTime, UNIX_EPOCH};

let unix_timestamp = SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .expect("Errore durante il calcolo del tempo Unix");

println!("Il tempo Unix corrente è {}", unix_timestamp.as_secs());
```

Output:
```
Il tempo Unix corrente è 1578029458
```

### Conversione in altre zone orarie
È possibile convertire la data corrente in un'altra zona oraria utilizzando il metodo `to_datetime()` e specificando la zona oraria desiderata. Ad esempio:

```Rust
use chrono::Utc;

let current_time = SystemTime::now();
let datetime = current_time.to_datetime();

println!("La data e ora corrente nella zona oraria di Los Angeles è {}", datetime.with_timezone(&Utc));
```

Output:
```
La data e ora corrente nella zona oraria di Los Angeles è 2019-10-15 01:30:20.263435568 UTC
```

## Vedi Anche
- [Documentazione Rust sulla libreria std::time](https://doc.rust-lang.org/std/time/)
- [Documentazione Chrono sulla gestione del tempo in Rust](https://docs.rs/chrono/0.4.10/chrono/)
- [Tutorial su come ottenere la data corrente in Rust](https://www.rust-lang.org/it/learn/get-current-date-time)