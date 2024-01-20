---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Convertire una stringa in minuscolo significa modificare tutte le lettere maiuscole nella stringa in lettere minuscole. I programmatori lo fanno spesso per standardizzare l'input utente, rendendo i dati più coerenti e facili da manipolare.

## Come fare:

In Rust, puoi convertire facilmente una stringa in minuscolo usando il metodo `to_lowercase`:

```Rust
let s = String::from("CIAO MONDO");
let t = s.to_lowercase();
println!("{}", t);
```
Questo ne sarà l'output:

```Rust
ciao mondo
```

## Approfondimento

In termini storici, la necessità di convertire le stringhe in minuscolo esiste fin dall'inizio dei computer che gestiscono il testo. Nel contesto della programmazione Rust, è stato fatto un grande lavoro per ottimizzare l'operazione di 'to_lowercase'.

Ci sono alternative al metodo `to_lowercase`. Una di queste è l'utilizzo della funzione `chars`, combinata con `map` e `collect`:

```Rust
let s = String::from("CIAO MONDO");
let t: String = s.chars().map(|c| c.to_lowercase().next().unwrap()).collect();
println!("{}", t);
```
Sebbene questo metodo possa sembrare più complicato, offre una maggiore flessibilità, permettendi di applicare operazioni differenti a ogni carattere della stringa.

Internamente, `to_lowercase` in Rust funziona consultando una tabella interna che mappa ogni carattere maiuscolo alla sua versione minuscola. Questo garantisce alta efficienza e velocità.

## Vedi Anche

Per ulteriori dettagli sulla manipolazione delle stringhe in Rust, dai un'occhiata a questi link:

1. [La documentazione ufficiale di Rust sulla stringa](https://doc.rust-lang.org/std/string/struct.String.html)
2. [Rust by Example: Stringa vs Str](https://doc.rust-lang.org/rust-by-example/std/str.html)