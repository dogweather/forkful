---
title:                "Utilizzo di array associativi"
aliases:
- it/rust/using-associative-arrays.md
date:                  2024-01-30T19:12:47.035062-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di array associativi"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Gli array associativi, o come li chiamano i Rustaceans "hash map", sono collezioni che memorizzano dati in coppie chiave-valore. I programmatori li utilizzano per una rapida ricerca dei dati, consentendo una manipolazione efficiente dei dati basata su chiavi uniche.

## Come fare:

In Rust, il tipo `HashMap` dal modulo `std::collections` fornisce la funzionalità degli array associativi. Ecco come puoi lavorarci:

```Rust
use std::collections::HashMap;

fn main() {
    // Creazione di un nuovo HashMap
    let mut scores = HashMap::new();

    // Inserimento valori
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    // Accesso ai valori
    let team_name = String::from("Blue");
    if let Some(score) = scores.get(&team_name) {
        println!("Punteggio per la squadra Blue: {}", score); // Output: Punteggio per la squadra Blue: 10
    }

    // Aggiornamento di un valore
    scores.entry(String::from("Blue")).and_modify(|e| *e += 5);

    // Iterazione sulle coppie chiave-valore
    for (key, value) in &scores {
        println!("{}: {}", key, value); // Output: Blue: 15, Yellow: 50
    }
}
```

## Approfondimento

L'`HashMap` in Rust utilizza una funzione di hashing per mappare le chiavi ai valori, il che consente un rapido recupero dei dati. Tuttavia, questa efficienza ha un costo: gli hash map non mantengono l'ordine dei loro elementi. Questo è in contrasto con altre implementazioni di array associativi, come quelle in Python (`dict`) o Ruby, che nelle versioni più recenti mantengono l'ordine di inserimento come caratteristica. Per i casi d'uso in cui l'ordine delle coppie chiave-valore è significativo, gli sviluppatori Rust potrebbero prendere in considerazione l'uso del `BTreeMap` dal modulo `std::collections`, che mantiene l'ordine ma potrebbe offrire un'inserimento e recupero più lenti rispetto a `HashMap`. In ultima analisi, la scelta tra `HashMap` e `BTreeMap` dipende da requisiti specifici relativi all'ordine e alle prestazioni.
