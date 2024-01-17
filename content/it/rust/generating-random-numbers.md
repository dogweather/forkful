---
title:                "Generazione di numeri casuali"
html_title:           "Rust: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

Cosa & Perché?

Generare dei numeri casuali è un modo per ottenere dei valori approssimativamente casuali all'interno di un programma. I programmatori spesso fanno uso di numeri casuali per generare dati di test, creare giochi o simulazioni e implementare algoritmi che richiedono input casuale.

```Rust
use rand::Rng;

fn main() {
    // Genera un numero casuale compreso tra 1 e 100
    let random_number = rand::thread_rng().gen_range(1, 101);
    println!("Il numero casuale generato è {}", random_number);
}
```

## Come fare:

Per generare numeri casuali in Rust, è necessario utilizzare la libreria `rand`. Questa libreria fornisce una serie di funzioni per generare numeri casuali di diversi tipi, come ad esempio `gen_range` che è utilizzata nell'esempio sopra. Per prima cosa, è necessario importare la libreria con `use rand::Rng;` e poi si può utilizzare la funzione `gen_range` per scegliere un intervallo di numeri da cui generare il numero casuale.

## Approfondimento:

Prima dell'introduzione della libreria `rand` in Rust, i programmatori dovevano utilizzare metodi non ufficiali per generare numeri casuali all'interno dei loro programmi. Questo spesso portava a risultati non sempre affidabili. Oggi, grazie alla libreria `rand`, è possibile generare numeri casuali in modo semplice e affidabile.

Altre alternative alla libreria `rand` sono ad esempio `rand_pcg` e `rand_xorshift`, che utilizzano altri algoritmi per generare numeri casuali.

Per quanto riguarda l'implementazione di `rand` in Rust, fa uso di generatori di numeri casuali pseudo-casuali, i quali non sono realmente casuali ma producono sequenze di numeri che appaiono casuali. La libreria `rand` sfrutta anche l'imprevedibilità dei valori di tempo di sistema per ottenere dei numeri casuali più affidabili.

## Vedi anche:

- Documentazione della libreria `rand`: https://docs.rs/rand
- Come generare numeri casuali in altri linguaggi di programmazione: https://blog.abelotech.com/posts/generating-random-numbers-programming-languages/
- Esempi di utilizzo della libreria `rand` in Rust: https://rust-random.github.io/book/intro.html