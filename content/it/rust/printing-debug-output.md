---
title:    "Rust: Stampa dell'output di debug"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

La stampa di output di debug è un'importante pratica di programmazione per identificare e risolvere eventuali errori nel codice. Inoltre, può essere utile per comprendere il flusso di esecuzione di un programma e per visualizzare i valori delle variabili durante l'esecuzione.

## Come fare

Per stampare output di debug in Rust, è possibile utilizzare la macro `println!()`. Questa macro funziona esattamente come `println!()` utilizzata per la stampa di output regolare, ma accetta anche espressioni di debug tra parentesi graffe `{}`. Ad esempio:

```Rust
let x = 5;
println!("Il valore di x è {}", x); // Stampa: Il valore di x è 5
```

È anche possibile stampare più espressioni di debug in una sola riga, separandole con una virgola. Ad esempio:

```Rust
let y = 10;
let z = 15;
println!("Il valore di y è {} e il valore di z è {}", y, z); // Stampa: Il valore di y è 10 e il valore di z è 15
```

È importante notare che le espressioni di debug possono essere qualsiasi cosa che implementi il trait `fmt::Debug`, tra cui anche i tipi personalizzati creati dall'utente.

## Approfondimenti

La macro `println!()` accetta anche una sintassi speciale per la stampa di output di debug più dettagliato. Ad esempio, utilizzando `"{:?}"` al posto di `"{}"`, verrà stampata un'espressione di debug di default più dettagliata. Inoltre, è possibile aggiungere una descrizione alla variabile, in questo modo `"{:?}: {}"`. Esempio:

```Rust
let nome = "Marco";
let eta = 30;
println!("Il mio nome è {:?} e ho {} anni", nome, eta); // Stampa: Il mio nome è "Marco" e ho 30 anni
```

## Vedi anche

- [Documentazione di Rust sulla macro `println!()`](https://doc.rust-lang.org/std/macro.println.html)
- [Articolo sulle espressioni di debug in Rust](https://www.digitalocean.com/community/tutorials/how-to-use-the-debug-macro-in-rust)
- [Esempio di utilizzo delle macro `println!()` e `format!()` per la formattazione di output in Rust](https://dev.to/n1arash/how-to-use-macros-to-format-print-output-in-rust-17h4)