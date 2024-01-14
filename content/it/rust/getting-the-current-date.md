---
title:    "Rust: Ottenere la data corrente"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore o semplicemente appassionato di tecnologia, potresti chiederti perché qualcuno vorrebbe conoscere la data attuale tramite un programma. La verità è che la data attuale è un'informazione molto utile per molte applicazioni, come ad esempio nella creazione di log o nella gestione di eventi programmati.

## Come Fare

In Rust, ottenere la data corrente è abbastanza semplice. Per farlo, dobbiamo utilizzare il modulo `std::time` e la sua funzione `now()`, come mostrato nell'esempio seguente:

```Rust
use std::time::SystemTime;

let now = SystemTime::now();
```

In questo modo, abbiamo ottenuto un valore che rappresenta il momento esatto in cui è stato chiamato il nostro programma. Per estrarre la data corrente da questo valore, possiamo utilizzare il metodo `to_local()` e la struttura `LocalDateTime`, come mostrato di seguito:

```Rust
use std::time::{SystemTime, Duration};
use chrono::{DateTime, Local};

let now = SystemTime::now();
let current_date = now.to_local();
let date_time = DateTime::<Local>::from(current_date);
```

Ora, `current_date` contiene un `chrono::LocalDateTime` che possiamo utilizzare per ottenere la data in diversi formati, ad esempio come stringa:

```Rust
println!("{}", date_time.format("%d/%m/%Y").to_string()); //stampa la data corrente nel formato dd/mm/yyyy
```

## Approfondimento

Se vuoi approfondire questo argomento, puoi guardare la documentazione ufficiale del modulo `std::time` e del crate `chrono`, che offre funzionalità aggiuntive per la gestione delle date e degli orari.

## Vedi Anche

- [Documentazione del modulo std::time](https://doc.rust-lang.org/std/time/index.html)
- [Documentazione del crate chrono](https://docs.rs/chrono/0.4.19/chrono/)