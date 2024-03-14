---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:40.997314-07:00
description: "Le espressioni regolari, o regex, permettono agli sviluppatori di cercare,\
  \ corrispondere e manipolare stringhe con tecniche avanzate di riconoscimento di\u2026"
lastmod: '2024-03-13T22:44:43.208061-06:00'
model: gpt-4-0125-preview
summary: "Le espressioni regolari, o regex, permettono agli sviluppatori di cercare,\
  \ corrispondere e manipolare stringhe con tecniche avanzate di riconoscimento di\u2026"
title: Utilizzo delle espressioni regolari
---

{{< edit_this_page >}}

## Cosa e perché?

Le espressioni regolari, o regex, permettono agli sviluppatori di cercare, corrispondere e manipolare stringhe con tecniche avanzate di riconoscimento di modelli. In Rust, l'utilizzo delle regex aiuta nell'efficace analisi ed elaborazione dei dati testuali, rendendo operazioni come la validazione dei dati, la ricerca e le trasformazioni di testo più snelle e gestibili.

## Come fare:

La libreria `regex` di Rust è la scelta principale per lavorare con le espressioni regolari. Per usarla, devi prima aggiungerla al tuo `Cargo.toml`:

```toml
[dependencies]
regex = "1"
```

Poi, puoi iniziare ad implementare le funzionalità delle regex nel tuo codice Rust. Ecco come eseguire alcune operazioni comuni:

### Corrispondenza di un Modello in una Stringa

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-15";

    println!("Il testo corrisponde al modello di data? {}", re.is_match(date));
    // Output: Il testo corrisponde al modello di data? true
}
```

### Trovare e Accedere alle Corrispondenze

```rust
use regex::Regex;

fn main() {
    let text = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(text) {
        println!("Linguaggio: {}, Anno: {}", &cap[1], &cap[2]);
    }
    // Output:
    // Linguaggio: Rust, Anno: 2023
    // Linguaggio: C++, Anno: 2022
    // Linguaggio: Python, Anno: 2021
}
```

### Sostituzione del Testo

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let text = "Rust 2023, C++ 2022, Python 2021";
    let replaced = re.replace_all(text, "$1 è stato aggiornato nel $2");

    println!("Testo aggiornato: {}", replaced);
    // Output: Testo aggiornato: Rust è stato aggiornato nel 2023, C++ è stato aggiornato nel 2022, Python è stato aggiornato nel 2021
}
```

### Divisione del Testo Usando una Regex

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // dividi per ogni carattere non di parola
    let text = "Rust-C++-Python-Go";

    let fields: Vec<&str> = re.split(text).collect();

    for field in fields {
        println!("Linguaggio: {}", field);
    }
    // Output:
    // Linguaggio: Rust
    // Linguaggio: C++
    // Linguaggio: Python
    // Linguaggio: Go
}
```

Questi esempi forniscono una guida di base per iniziare con le espressioni regolari in Rust. Man mano che le tue esigenze diventano più sofisticate, il crate `regex` offre una vasta gamma di funzionalità per compiti complessi di corrispondenza di modelli e manipolazione del testo.
