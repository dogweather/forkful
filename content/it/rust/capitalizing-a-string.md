---
title:    "Rust: Capitalizzare una stringa"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Perché

Molte volte while working with strings in Rust, ci può essere la necessità di convertire il testo in maiuscolo. Ci sono varie ragioni per fare questo, come il corretto formataggio dei dati o la presentazione della stringa all'utente. In questo post, esploreremo come convertire una stringa in maiuscolo utilizzando il linguaggio di programmazione Rust.

## Come Fare

Il metodo più semplice per convertire una stringa in maiuscolo in Rust è utilizzando il metodo `to_uppercase()`. Questo metodo prende in input una stringa e restituisce una nuova stringa con tutti i caratteri convertiti in maiuscolo.

```Rust
let stringa = String::from("ciao a tutti!");
let nuova_stringa = stringa.to_uppercase();
println!("{}", nuova_stringa);
```

Questo codice produce l'output "CIAO A TUTTI!".

Oltre al metodo `to_uppercase()`, possiamo anche utilizzare il metodo `to_ascii_uppercase()` per convertire i caratteri in codice ASCII in maiuscolo e il metodo `to_lowercase()` per convertire in lettere minuscole.

```Rust
let stringa = "Hello World!";
let maiuscola = stringa.to_ascii_uppercase();
let minuscola = stringa.to_lowercase();

println!("{}, {}, {}", stringa, maiuscola, minuscola);
```

Questo codice produce l'output "Hello World!, HELLO WORLD!, hello world!".

## Deep Dive

Ora che abbiamo visto il metodo più semplice per convertire una stringa in maiuscolo, è utile comprendere come funziona il processo di conversione. In Rust, le stringhe sono immutabili, il che significa che non possono essere modificate direttamente. Quando usiamo il metodo `to_uppercase()`, in realtà viene creato una nuova stringa con tutti i caratteri in maiuscolo e questa viene restituita.

Inoltre, questo metodo funziona solo per le lingue supportate da Unicode. Se utilizzi lingue non supportate, dovrai utilizzare una soluzione personalizzata per convertire la stringa in maiuscolo.

## Vedi anche

- [Documentazione ufficiale di Rust sulla gestione delle stringhe](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Altre funzioni utili per la manipolazione delle stringhe in Rust](https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase)
- [Rust Playground per provare i codici di esempio](https://play.rust-lang.org/)