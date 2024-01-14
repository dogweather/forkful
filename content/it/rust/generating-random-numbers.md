---
title:                "Rust: Generazione di numeri casuali"
programming_language: "Rust"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Scegliere di generare numeri casuali è una scelta comune per molti programmatori, poiché spesso è necessario per creare algoritmi o testare funzionalità di un programma. Imparare come farlo in Rust può essere molto utile per chi vuole sviluppare in questo linguaggio.

## Come fare

Rust offre diversi modi per generare numeri casuali, ma il più comune è utilizzare il modulo `rand`. Per iniziare, è necessario importare il modulo nel tuo codice.

```Rust
use rand::Rng;
```

Una volta importato, puoi utilizzare il metodo `gen_range` per generare un numero casuale compreso tra due valori specificati. Ad esempio, se volessi generare un numero casuale compreso tra 1 e 10, il codice sarebbe il seguente:

```Rust
let num = rand::thread_rng().gen_range(1, 11);
println!("Il numero casuale è: {}", num);
```

Il metodo `gen_range` utilizza un generatore di numeri casuale globale chiamato `thread_rng` per ottenere i numeri casuali. Ciò garantisce che i numeri generati siano veramente casuali. Inoltre, è possibile specificare anche un terzo parametro per indicare il passo tra i numeri generati.

## Approfondimento

Se vuoi approfondire ulteriormente il funzionamento dei numeri casuali in Rust, è possibile utilizzare il modulo `rand` per generare numeri secondo diverse distribuzioni, come la distribuzione di Gauss o di Poisson. Inoltre, è possibile utilizzare un generatore di numeri casuale non basato sulla libreria standard di Rust, come ad esempio `rand_pcg`, che offre un miglior controllo sui numeri generati.

## Vedi anche

- [La documentazione ufficiale di Rust su generazione di numeri casuali](https://doc.rust-lang.org/rand/rand/index.html)
- [Un tutorial su come generare numeri casuali in Rust](https://www.youtube.com/watch?v=qSvDcx59wLU)
- [Una raccolta di esempi pratici di generazione di numeri casuali in Rust](https://rust-random.github.io/book/guide-start.html)