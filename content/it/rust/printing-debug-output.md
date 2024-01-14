---
title:                "Rust: Stampa degli output di debug."
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare messaggi di debug è cruciale per comprendere le operazioni interne del nostro codice e individuare eventuali errori. Inoltre, è uno strumento utile per la risoluzione dei problemi e il miglioramento delle performance del nostro programma.

## Come Fare

Per stampare messaggi di debug in Rust, possiamo utilizzare la funzione `println!` seguita dalle variabili o espressioni che vogliamo visualizzare tra parentesi graffe. Ad esempio:

```Rust
let x = 10;
println!("Il valore di x è {}", x);
// Output: Il valore di x è 10
```

Possiamo anche utilizzare la macro `dbg!` che stampa non solo il valore della variabile, ma anche il nome della variabile stessa per facilitare la comprensione del codice. Ad esempio:

```Rust
let name = "Carlo";
dbg!(name);
// Output: name = "Carlo"
```

Possiamo anche stampare più variabili o espressioni separandole da una virgola all'interno delle parentesi graffe. Ad esempio:

```Rust
let a = 5;
let b = 2;
println!("La somma di {} e {} è {}", a, b, a + b);
// Output: La somma di 5 e 2 è 7
```

## Approfondimento

Esistono diverse opzioni per stampare messaggi di debug in maniera più avanzata. Possiamo utilizzare la funzione `eprintln!` per stampare il messaggio di errore su stderr anziché su stdout. Inoltre, con il formato di debug `{:?}` possiamo stampare tutte le informazioni di una struttura dati o un vettore in una sola riga anziché doverle elencare singolarmente. Ad esempio:

```Rust
let numbers = vec![1, 2, 3];
println!("{:?}", numbers);
// Output: [1, 2, 3]
```

Possiamo anche utilizzate la funzione `panic!` per interrompere l'esecuzione e stampare un messaggio di errore personalizzato. Questo può essere utile per gestire situazioni di errore e debug in maniera efficace.

## Vedi Anche

- [Guida completa su come stampare messaggi di debug in Rust](https://www.rust-lang.org/learn/get-started#understanding-types)
- [Documentazione ufficiale di println! macro](https://doc.rust-lang.org/std/macro.println.html)
- [Tutorial su come utilizzare dbg! macro per il debug in Rust](https://serokell.io/blog/rust-debugging-tools)