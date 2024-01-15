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

## Perché

Generare numeri casuali è spesso necessario per molti tipi di programmazione, come giochi, simulazioni e criptografia.

## Come fare

Generare numeri casuali in Rust è semplice e diretto. Basta importare il modulo della libreria standard `rand` e utilizzarlo per generare un numero casuale, come nel seguente esempio:

```Rust
use rand::Rng;

fn main() {
    let num: u32 = rand::thread_rng().gen_range(1, 100);
    println!("Number generated: {}", num);
}
```

Questo codice utilizza il metodo `gen_range` del modulo `Rng` per generare un numero casuale compreso tra 1 e 100. È importante notare che il numero viene generato utilizzando un generatore di numeri casuale "sicuro" (thread-safe), che garantisce una maggiore casualità e non può essere influenzato dall'esecuzione del programma.

Altri modi per generare numeri casuali in Rust includono l'utilizzo di `thread_rng().gen()` per generare un numero intero in un intervallo specificato e `thread_rng().gen_bool()` per generare un booleano casuale. È anche possibile specificare il tipo di dato che si desidera generare, come `f32` o `char`.

## Scoprire in profondità

La libreria `rand` di Rust offre una varietà di generatori di numeri casuali, inclusi alcuni che possono essere personalizzati per soddisfare le esigenze specifiche del programma. Per esempio, è possibile utilizzare il modulo `distributions` per generare numeri casuali secondo una distribuzione specifica, come la distribuzione normale o la distribuzione esponenziale. Inoltre, la libreria `rand` supporta anche la generazione di numeri casuali criptograficamente sicuri utilizzando il modulo `isahc_rng`, per garantire la massima sicurezza nel caso in cui ci sia bisogno di generare numeri casuali per scopi critici.

Un'altra opzione interessante offerta dalla libreria `rand` è la possibilità di utilizzare un seme (seed) personalizzato per il generatore di numeri casuali, che può essere utile per riprodurre gli stessi risultati ogni volta che si esegue il programma.

## Vedi anche

- Documentazione ufficiale di Rust sulla generazione di numeri casuali: https://doc.rust-lang.org/stable/book/ch07-08-doing-automated-tests.html#generating-random-numbers
- Esempio di utilizzo della libreria `rand` per generare una password casuale: https://gist.github.com/Golony/baf7c8fe079a0b5748a0