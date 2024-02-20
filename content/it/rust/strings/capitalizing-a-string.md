---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:28.696646-07:00
description: "Capitalizzare una stringa in Rust comporta la modifica della stringa\
  \ in modo che il suo primo carattere sia una lettera maiuscola, lasciando inalterata\
  \ il\u2026"
lastmod: 2024-02-19 22:05:02.262323
model: gpt-4-0125-preview
summary: "Capitalizzare una stringa in Rust comporta la modifica della stringa in\
  \ modo che il suo primo carattere sia una lettera maiuscola, lasciando inalterata\
  \ il\u2026"
title: Capitalizzare una stringa
---

{{< edit_this_page >}}

## Cosa e Perché?

Capitalizzare una stringa in Rust comporta la modifica della stringa in modo che il suo primo carattere sia una lettera maiuscola, lasciando inalterata il resto della stringa. I programmatori spesso eseguono questa operazione per scopi di formattazione, come la preparazione di parole per titoli o per garantire la coerenza nell'input dell'utente.

## Come fare:

Per capitalizzare una stringa in Rust, hai due percorsi principali: utilizzare le funzionalità della libreria standard o impiegare crate di terze parti per esigenze più complesse o specifiche. Ecco come puoi fare entrambi.

### Utilizzando la Libreria Standard di Rust

La libreria standard di Rust non fornisce un metodo diretto per capitalizzare le stringhe, ma puoi raggiungere questo scopo manipolando i caratteri della stringa.

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hello";
    println!("{}", capitalize_first(my_string)); // Output: Hello
}
```

### Utilizzando il Crate `heck`

Per un approccio più diretto, specialmente quando si lavora in un contesto di elaborazione di testi più ampio, potresti preferire l'uso di librerie di terze parti come `heck`. Il crate `heck` offre varie funzionalità di conversione di case, inclusa una semplice modalità per capitalizzare le stringhe.

Prima di tutto, aggiungi `heck` al tuo `Cargo.toml`:

```toml
[dependencies]
heck = "0.4.0"
```

Poi, usalo per capitalizzare la tua stringa:

```rust
extern crate heck; // Non necessario in Rust edizione 2018 o successive
use heck::TitleCase;

fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // Output: Hello World
}
```

Nota: Il metodo `to_title_case` fornito da `heck` capitalizza ogni parola nella stringa, il che potrebbe essere più di quello che stai cercando se vuoi solo il primo carattere della stringa capitalizzato. Regola il tuo utilizzo in base alle tue esigenze specifiche.
