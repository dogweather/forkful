---
title:                "Rust: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si lavora con stringhe in un programma, potrebbe essere necessario estrarre una porzione specifica di una stringa. Ad esempio, potresti voler estrarre il nome di un file da un'URL o ottenere solo il codice di un paese da un indirizzo e-mail. In situazioni come queste, estrarre substringhe diventa fondamentale per gestire efficacemente i dati. In questo articolo, impareremo come fare questo in Rust.

## Come fare

Per estrarre una sottocatena da una stringa in Rust, utilizziamo il metodo `slice`, che accetta due argomenti: l'indice di inizio e l'indice di fine della sottocatena desiderata. Ecco un esempio di come utilizzarlo:

```Rust
let my_string = "Questo è un esempio di stringa da cui voglio estrarre una sottocatena.";

let substring = my_string.slice(8, 21);

println!("{}", substring); // Output: "un esempio di"
```

In questo esempio, abbiamo utilizzato il metodo `slice` per estrarre una sottocatena dalla nostra stringa a partire dall'ottavo carattere e fino al ventunesimo. Il carattere finale, come per gli slice in generale, non viene incluso nella sottocatena risultante.

## Approfondimento

Oltre al metodo `slice`, esistono anche altri modi per estrarre substringhe in Rust. Ad esempio, possiamo utilizzare la funzione `split` per dividere una stringa in una collezione di substringhe in base a un delimitatore specifico. Ecco un esempio:

```Rust
let my_string = "Mario-Rossi-John";

let name_list: Vec<&str> = my_string.split("-").collect();

println!("{:?}", name_list); // Output: ["Mario", "Rossi", "John"]
```

In questo caso, abbiamo utilizzato la funzione `split` per dividere la stringa utilizzando il carattere `-` come delimitatore. Il risultato è una collezione di tre substringhe, contenenti rispettivamente i nomi "Mario", "Rossi" e "John".

## Vedi anche

Per ulteriori informazioni sul lavoro con stringhe in Rust, puoi consultare le seguenti risorse:

- [Documentazione ufficiale di Rust](https://www.rust-lang.org/it/learn)
- [Video tutorial su gestione delle stringhe in Rust](https://www.youtube.com/watch?v=ArXP-g3331w)
- [Approfondimenti su gestione delle stringhe in Rust](https://blog.thoughtram.io/string-handling-in-rust/)