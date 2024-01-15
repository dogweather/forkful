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

## Perché

Se stai cercando un modo semplice e veloce per convertire una stringa in maiuscolo, allora il linguaggio Rust ha la soluzione perfetta per te. Con la sua sintassi chiara e potente, capitalizzare una stringa in Rust è facile e intuitivo.

## Come Fare

Per capitalizzare una stringa in Rust, è necessario utilizzare il metodo `to_ascii_uppercase()`. Ecco un esempio di come utilizzarlo:

```Rust
let str = "ciao mondo";
let str_in_maiuscolo = str.to_ascii_uppercase();
println!("{}", str_in_maiuscolo); // stampa "CIAO MONDO"
```

## Approfondimento

In Rust, le stringhe sono immutabili, il che significa che una volta definita una stringa, non è possibile modificarla direttamente. Invece, dobbiamo utilizzare metodi come `to_ascii_uppercase()` per creare una nuova stringa con la lettera maiuscola.

È importante notare che questo metodo funziona solo per le stringhe ASCII, quindi se la tua stringa contiene caratteri unicode, dovrai utilizzare il metodo `to_uppercase()` invece di `to_ascii_uppercase()`.

Inoltre, è possibile specificare un alfabeto locale per la conversione della stringa con il metodo `to_uppercase_with_locale()`, che accetta come parametro il codice del paese (ad esempio "IT" per l'Italia).

## Vedi Anche

Per ulteriori informazioni sulle stringhe in Rust, puoi consultare la documentazione ufficiale [qui](https://doc.rust-lang.org/std/string/index.html). Per imparare altre funzionalità utili del linguaggio, puoi consultare la nostra guida su [Come iniziare con Rust](https://www.digitalocean.com/community/tutorials/how-to-get-started-with-rust).