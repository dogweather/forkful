---
title:                "Convertire una data in una stringa"
html_title:           "Rust: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Se stai lavorando con le date in un programma Rust, potresti aver bisogno di convertire una data in una stringa per scopi di visualizzazione o di salvataggio dei dati. In questo articolo, ti mostreremo come fare proprio questo utilizzando il linguaggio Rust.

## Come fare

Per prima cosa, dovrai impostare la tua applicazione Rust per utilizzare il modulo `chrono`, che fornisce funzionalità per lavorare con le date e le ore. Per farlo, aggiungi `chrono` alla sezione `[dependencies]` del tuo file `Cargo.toml`: 

```Rust
[dependencies]
chrono = "0.4"
```

Una volta fatto ciò, importa il modulo `chrono` all'inizio del tuo file di codice:
```Rust
use chrono::{DateTime, Utc, TimeZone};
```

Per convertire una data in una stringa, è necessario creare un oggetto `DateTime` con la data desiderata. Ad esempio, per ottenere la data e l'ora attuali, puoi utilizzare il metodo `Utc::now()` come segue:
```Rust
let now: DateTime<Utc> = Utc::now();
```

Successivamente, utilizza il metodo `format()` sull'oggetto `DateTime` per specificare il formato della stringa di output. Ad esempio, per stampare una data nel formato "gg/mm/aaaa", puoi utilizzare il seguente codice:
```Rust
let date_string = now.format("%d/%m/%Y").to_string();
```

Infine, puoi utilizzare la variabile `date_string` nel tuo codice come desideri, ad esempio per stamparla a schermo o salvarla in un file.

Ecco un esempio completo di come convertire una data in una stringa in Rust:
```Rust
use chrono::{DateTime, Utc, TimeZone};

fn main() {
    let now: DateTime<Utc> = Utc::now();
    let date_string = now.format("%d/%m/%Y").to_string();
    println!("{}", date_string);
}
```

L'output dovrebbe essere la data attuale nel formato "gg/mm/aaaa".

## Approfondimento

Ci sono molti altri formati di stringa che puoi utilizzare utilizzando il metodo `format()`, come ad esempio "%Y-%m-%d" per una data nel formato "aaaa-mm-gg" o "%H:%M:%S" per un'ora nel formato "hh:mm:ss". Puoi anche combinare più specificatori di formato per creare una stringa personalizzata.

Inoltre, il pacchetto `chrono` offre funzionalità avanzate per lavorare con date, come la gestione dei fusi orari e il supporto per calendari non gregoriani. Assicurati di consultare la documentazione ufficiale per ulteriori informazioni sugli utilizzi avanzati di questo modulo.

## Vedi anche

- Documentazione ufficiale di `chrono`: https://docs.rs/chrono/
- Esempi aggiuntivi di conversione di date in stringhe in Rust: https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=479851a3e0e1ea3309370186ae6e499c