---
title:                "Cancellazione di caratteri corrispondenti ad un modello"
html_title:           "Rust: Cancellazione di caratteri corrispondenti ad un modello"
simple_title:         "Cancellazione di caratteri corrispondenti ad un modello"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Capita spesso che ci troviamo di fronte alla necessità di cancellare caratteri in una stringa che corrispondono a un certo pattern. Ciò può essere utile quando si vuole pulire una stringa da eventuali caratteri superflui o ridurre la sua lunghezza.

## How To

Per eseguire questa operazione in Rust, possiamo utilizzare il metodo `retain()` associato alle stringhe.

```
Rust let mut stringa = String::from("Ciao, 12345!");
stringa.retain(|c| c.is_alphabetic()); //il carattere "1" e "2" saranno cancellati
println!("{}", stringa); // output: "Ciao!"
```

In questo esempio, abbiamo utilizzato la funzione `is_alphabetic()` per determinare quali caratteri mantenere nella stringa. Possiamo anche utilizzare altri metodi come `is_numeric()` o `is_whitespace()` per specificare ulteriormente quali caratteri eliminare.

## Deep Dive

Il metodo `retain()` è definito all'interno della struttura `String` in Rust. Esso accetta una funzione come argomento e controlla ogni carattere della stringa corrente. Se la funzione restituisce `true`, il carattere verrà mantenuto, altrimenti verrà eliminato.

È importante notare che, a differenza di altri metodi come `replace()`, `retain()` agisce direttamente sulla stringa e modifica il suo contenuto anziché restituire una nuova stringa.

## See Also

- [La documentazione ufficiale di Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.retain)
- [Un esempio di utilizzo di `retain()`](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=aadadca8fc23ff32dc1e501d9fed151b)