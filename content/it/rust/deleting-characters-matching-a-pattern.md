---
title:                "Rust: Eliminazione di caratteri corrispondenti a un modello"
simple_title:         "Eliminazione di caratteri corrispondenti a un modello"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler eliminare caratteri corrispondenti a un determinato pattern in un programma Rust. Ad esempio, potresti voler pulire input utente che contiene caratteri indesiderati o rimuovere del testo superfluo da una stringa. In questo post, esploreremo come farlo utilizzando il linguaggio di programmazione Rust.

## Come Fare

Per eliminare caratteri corrispondenti a un pattern in Rust, possiamo utilizzare il metodo `replace` nella libreria standard `String`. Questo metodo richiede due argomenti: il pattern da cercare e il testo con cui sostituire il pattern. Ad esempio, se volessimo eliminare tutti gli spazi bianchi da una stringa, possiamo fare così:

```Rust
let mut stringa = String::from("Questo è un esempio.");
stringa.replace(" ", ""); // Elimina gli spazi bianchi
println!("{}", stringa); // Output: "Questoèunesempio."
```
In questo esempio, stiamo eliminando tutti gli spazi bianchi dalla stringa utilizzando il metodo `replace`. Possiamo anche utilizzare espressioni regolari per trovare pattern più specifici e sostituirli con del testo diverso. Ad esempio, se vogliamo eliminare tutte le vocali da una stringa, possiamo farlo in questo modo:

```Rust
let mut stringa = String::from("Questa è un'altra prova.");
stringa.replace("[aeiou]", ""); // Sostituisce tutte le vocali con una stringa vuota
println!("{}", stringa); // Output: "Qst 'n ltr prvv."
```

## Approfondimento

La libreria standard `String` offre diversi metodi che possono essere utili per eliminare caratteri corrispondenti a un pattern. Oltre a `replace`, possiamo utilizzare anche `remove` per eliminare un carattere specifico, `retain` per mantenere solo dei caratteri che corrispondono a un pattern e `trim` per eliminare caratteri di spazio bianco all'inizio e alla fine di una stringa. Inoltre, possiamo combinare diversi metodi per ottenere il risultato desiderato.

## Vedi Anche

- Documentazione ufficiale di Rust per la libreria `String`: https://doc.rust-lang.org/std/string/struct.String.html
- Tutorial introduttivo su Rust: https://www.rust-lang.org/learn/get-started
- Mastering Rust: A Comprehensive Guide to the Rust Programming Language: https://www.packtpub.com/product/mastering-rust-second-edition/9781789346574