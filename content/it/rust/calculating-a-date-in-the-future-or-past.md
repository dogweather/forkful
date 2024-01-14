---
title:    "Rust: Calcolare una data nel futuro o nel passato"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molti motivi per cui qualcuno potrebbe voler calcolare una data in futuro o in passato, come per esempio per verificare quando un determinato evento potrebbe accadere o per pianificare una vacanza. In ogni caso, imparare come fare questo tipo di calcoli utilizzando Rust può essere molto utile e divertente.

## Come

Per calcolare una data in futuro o in passato in Rust, dobbiamo prima di tutto importare il modulo `chrono`, che ci permette di gestire date e orari in modo semplice e intuitivo. Inoltre, dovremo utilizzare i tipi di dato `DateTime` e `Duration` per fare i nostri calcoli.

```Rust
use chrono::{DateTime, Duration, Utc}; // Importa il modulo chrono
```

Per esempio, se vogliamo ottenere la data di oggi, possiamo utilizzare il metodo `now()` e specificare il fuso orario `Utc`:

```Rust
let oggi = Utc::now();
```

Per calcolare una data in futuro o in passato, dobbiamo utilizzare il metodo `checked_add()` o `checked_sub()` della struttura `DateTime`. Questi metodi ci permettono di aggiungere o sottrarre una `Duration` dalla data corrente. Vediamo un esempio pratico:

```Rust
let oggi = Utc::now(); // Data di oggi
let un_mese = Duration::days(30); // Durata di 30 giorni
let tra_un_mese = oggi.checked_add(un_mese); // Data tra un mese
```

Possiamo anche utilizzare il metodo `format()` per formattare la data secondo il nostro gusto, per esempio per ottenere una stringa nella forma `giorno/mese/anno`:

```Rust
let data = oggi.format("%d/%m/%Y");
println!("La data di oggi è: {}", data); // Output: La data di oggi è: 10/06/2021
```

## Deep Dive

Se vogliamo andare più in profondità, possiamo approfondire il concetto di data e orario in Rust. In particolare, la struttura `DateTime` è composta da tre parti: data, orario e fuso orario. Possiamo ottenere ciascuna di queste parti utilizzando i metodi `date()`, `time()` e `timezone()` rispettivamente.

Inoltre, possiamo anche convertire una data in un timestamp utilizzando il metodo `timestamp()` e specificando il fuso orario:

```Rust
let oggi = Utc::now(); // Data di oggi
let timestamp = oggi.timestamp(); // Timestamp di oggi
println!("La data di oggi in timestamp è: {}", timestamp); // Output: La data di oggi in timestamp è: 1623334978
```

## Vedi anche

* Documentazione ufficiale di `chrono`: https://docs.rs/chrono/
* Tutorial su come calcolare una data in Rust: https://rosettacode.org/wiki/Relative_date_calculations#Rust
* Approfondimento sulla gestione delle date in Rust: https://blog.logrocket.com/dates-and-times-in-rust/