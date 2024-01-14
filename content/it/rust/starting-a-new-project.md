---
title:    "Rust: Avviare un nuovo progetto"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Perché

Se siete come me, vi ritroverete spesso a voler iniziare un nuovo progetto di programmazione come hobby o per lavoro. Ma perché dovremmo scegliere di usare Rust per questo? Beh, Rust è un linguaggio di programmazione moderno e sicuro che offre prestazioni ad alte prestazioni e una gestione della memoria affidabile. Inoltre, ha una comunità attiva e una documentazione eccellente.

## Come fare

Per iniziare a utilizzare Rust, prima di tutto dobbiamo installarlo sul nostro sistema. Per fare ciò, possiamo visitare il sito ufficiale di Rust e seguire le istruzioni di installazione. Una volta installato, possiamo utilizzare il comando `cargo new` per creare una nuova cartella del progetto. Successivamente, possiamo aprire il progetto nel nostro editor di testo preferito ed iniziare a scrivere il nostro codice Rust. Ecco un esempio di come potrebbe apparire il nostro primo programma Rust:

```Rust
fn main() {
    println!("Ciao mondo!"); //in Rust, per stampare a schermo utilizziamo il comando println!
}
```

Se eseguiamo questo programma, dovremmo ottenere il seguente output:

```
Ciao mondo!
```

Non è così difficile, vero? Rust ha una sintassi semplice e intuitiva che faciliterà la scrittura del nostro codice. Inoltre, possiamo utilizzare il comando `cargo run` per compilare ed eseguire il nostro programma in una sola volta.

## Approfondimento

Ora che abbiamo una base solida, possiamo approfondire un po' di più. Rust utilizza il concetto di *ownership* per gestire la memoria in modo sicuro e senza rischi di errori come i blocchi di memoria o le fughe di memoria. Grazie a questo, il processo di debugging risulta molto più semplice e veloce. Inoltre, Rust ha un sistema di tipi molto potente, che ci permette di scrivere codice robusto e sicuro. Anche la gestione degli errori è molto migliore rispetto ad altri linguaggi di programmazione, grazie all'utilizzo di `Result<T, E>` e `Option<T>`.

## Vedi anche

* [Sito ufficiale di Rust](https://www.rust-lang.org/it/)
* [Documentazione di Rust](https://doc.rust-lang.org/book/)
* [Rust Playground - un ambiente online per provare il codice Rust](https://play.rust-lang.org/)
* [Progetti di esempio in Rust](https://github.com/rust-lang/rust/tree/master/src/test/ui)