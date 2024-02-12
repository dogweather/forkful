---
title:                "Generazione di numeri casuali"
aliases: - /it/rust/generating-random-numbers.md
date:                  2024-01-27T20:35:34.052385-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generazione di numeri casuali"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Generare numeri casuali in Rust comporta l'utilizzo di librerie per produrre valori numerici imprevedibili, cosa indispensabile per compiti che vanno dalla crittografia e le simulazioni ai giochi e agli algoritmi randomizzati.

## Come fare:

Rust si affida a crate esterni per la generazione di numeri casuali, essendo `rand` il più comunemente utilizzato. Per iniziare a generare numeri casuali, dovrai prima aggiungere `rand` al tuo file `Cargo.toml`:

```toml
[dependencies]
rand = "0.8.5"
```

Successivamente, puoi generare numeri casuali usando `rand` nel tuo codice Rust. Ecco un esempio di generazione di un numero intero casuale e di un numero in virgola mobile:

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    
    // Genera un numero intero casuale tra 1 e 10
    let random_int: i32 = rng.gen_range(1..=10);
    println!("Numero intero casuale: {}", random_int);
    
    // Genera un numero in virgola mobile casuale tra 0.0 e 1.0
    let random_float: f64 = rng.gen::<f64>();
    println!("Numero in virgola mobile casuale: {}", random_float);
}
```

Un esempio di output potrebbe essere:

```plaintext
Numero intero casuale: 7
Numero in virgola mobile casuale: 0.9401077112175732
```

Si noti che rieseguire il programma produrrà valori diversi.

## Approfondimento

La generazione di numeri casuali in Rust, facilitata attraverso `rand` e le sue dipendenze come `getrandom`, rappresenta un'ampia astrazione sulle strutture del sistema operativo e sui generatori algoritmici. Storicamente, la casualità nel calcolo è evoluta da algoritmi semplici e prevedibili a metodi complessi e criptograficamente sicuri. L'approccio di Rust incapsula questa evoluzione attraverso il suo trait `Rng` personalizzabile, che può essere supportato da vari generatori in base alla qualità di casualità e alle prestazioni richieste.

Per la maggior parte delle applicazioni, fare affidamento su `rand` e sul RNG del sistema offre un buon equilibrio tra semplicità ed entropia. Tuttavia, per le applicazioni crittografiche, crate come `rand` si affidano a `getrandom` per l'inizializzazione, che a sua volta si basa su meccanismi specifici del SO (ad es., `/dev/urandom` sui sistemi simili a Unix), garantendo casualità criptograficamente sicura.

Alternativamente, se hai esigenze specifiche non soddisfatte da `rand`, esplorare altre crate o implementare generatori personalizzati basati su modelli matematici potrebbe essere una via da percorrere. Tuttavia, per la stragrande maggioranza dei casi d'uso, `rand` e il suo ecosistema offrono soluzioni robuste che sono sia efficienti che semplici da integrare nelle applicazioni Rust.
