---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Eliminare Caratteri Corrispondenti a un Modello in Rust

## Che Cosa e Perché?

Eliminare caratteri corrispondenti a un modello significa rimuovere tutte le occorrenze di un determinato set di caratteri da una stringa. I programmatori lo fanno per ripulire i dati, normalizzare le stringhe o semplificare i test di confronto.

## Come Fare:

Ecco un esempio di codice che mostra come eliminare i caratteri corrispondenti a un modello usando Rust:

```Rust
fn main() {
    let mut s = String::from("Hello, World!");
    let chars_to_remove: &[char] = &['l', ','];
    s.retain(|c| !chars_to_remove.contains(&c));
    println!("{}", s);
}
```

Questo darà come output:

```Rust
Heo Word!
```

## Approfondimento

Rust non aveva originariamente un metodo diretto per eliminare i caratteri corrispondenti a un modello. Questo è cambiato con l'aggiunta del metodo `retain` nella versione 1.53.0, che ha reso il processo molto più agevole.

Una valida alternativa è usare delle espressioni regolari. Tuttavia, questo necessiterà dell'aggiunta della crate `regex`, il che potrebbe aggiungere un overhead innecessario per operazioni di stringhe relativamente semplici.

Il metodo `retain` funziona iterando attraverso la stringa e mantenendo solo i caratteri che non matchano il pattern. Questo è un'operazione `O(n)`, il che la rende molto efficiente.

## Vedi Anche:

- Documentazione ufficiale di Rust sul metodo `retain`: [https://doc.rust-lang.org/std/string/struct.String.html#method.retain](https://doc.rust-lang.org/std/string/struct.String.html#method.retain)
- Rust by Example - "String": [https://doc.rust-lang.org/rust-by-example/std/str.html](https://doc.rust-lang.org/rust-by-example/std/str.html)
- Community di Rust su StackOverflow: [https://stackoverflow.com/questions/tagged/rust](https://stackoverflow.com/questions/tagged/rust)