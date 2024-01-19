---
title:                "Capitalizzare una stringa"
html_title:           "Rust: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Capitalizzare una stringa significa rendere maiuscola la prima lettera. I programmatori lo fanno per vari motivi - ad esempio, per migliorare l'estetica del testo o per soddisfare le convenzioni di formattazione.

## Come si fa:

Ecco un esempio su come capitalizzare una stringa in Rust.

```Rust
fn main() {
    let mut s = String::from("ciao mondo");
    let uppercase = s.to_uppercase();
    println!("{}", uppercase);
}
```
Se esegui questo codice, la tua uscita sarà:`CIAO MONDO`.

## Approfondimento

**1. Contesto storico:** Rust attualmente non offre una funzione nativa per capitalizzare solo la prima lettera di una stringa. Quindi, dobbiamo fare un po' di lavoro extra per ottenere quel risultato.

**2. Alternative:** In Rust puoi capitalizzare una stringa utilizzando il metodo `to_uppercase`. Però, se vuoi solo capitalizzare la prima lettera, anche questa è un'opzione:

```Rust
fn main() {
    let s = String::from("ciao mondo");
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().chain(chars).collect(),
    }
}
```

**3. Dettagli di implementazione:** Il metodo `to_uppercase()` ritorna una nuova `String` con tutti i caratteri maiuscoli. Per ottenere solamente la prima lettera maiuscola, dobbiamo estrarre la prima lettera, capitalizzarla e riassemblare la stringa.

## Vedi anche:

Per approfondire sulle stringhe in Rust, consigliamo di dargli un'occhiata alla documentazione ufficiale: [std::string::String - Rust](https://doc.rust-lang.org/std/string/struct.String.html)

Per un'introduzione più dettagliata su Rust, consigliamo "The Rust Programming Language" di Steve Klabnik e Carol Nichols: [The Rust Programming Language - an overview](https://doc.rust-lang.org/book/title-page.html)

Se vuoi esercitarti, dai un'occhiata alla sezione "stringhe" su Exercism: [Exercism: Rust Track](https://exercism.io/tracks/rust)