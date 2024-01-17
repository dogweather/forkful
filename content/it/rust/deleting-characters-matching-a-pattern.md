---
title:                "Eliminazione dei caratteri corrispondenti ad un modello"
html_title:           "Rust: Eliminazione dei caratteri corrispondenti ad un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti ad un modello"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La cancellazione di caratteri che corrispondono ad un determinato pattern è una pratica comune tra i programmatori per rimuovere parti di testo indesiderate nei loro codici. Ad esempio, potresti voler eliminare tutti i commenti o spazi vuoti da un file di testo per renderlo più leggibile e meno verboso.

## Come fare:
Ecco come puoi eseguire la cancellazione di caratteri che corrispondono ad un pattern in Rust:

```Rust
use regex::Regex;

fn main() {
    // Definisci il tuo pattern
    let pattern = Regex::new(r"([A-Z]+)\s+([0-9]+)").unwrap();
    
    // Crea una stringa su cui eseguire la cancellazione
    let testo = "COD123 abc456 DEF789";
    
    // Esegui la cancellazione del pattern
    let nuovo_testo = pattern.replace_all(testo, "$1$2");
    
    println!("{}", nuovo_testo); 
}
```

Questo codice userà una libreria chiamata `regex` per definire un pattern e utilizzarlo per sostituire ogni corrispondenza con una stringa vuota. Nel nostro esempio, vogliamo rimuovere i caratteri dalla stringa iniziale e ottenere "COD123abc456DEF789" come output.

## Approfondimento:
La cancellazione di caratteri che corrispondono ad un pattern è spesso vista come un'alternativa più efficiente rispetto alla rimozione di singoli caratteri da una stringa. Inoltre, questa pratica è diventata più popolare con l'uso diffuso delle espressioni regolari nelle moderne programmazioni.

Per implementare questa funzione in Rust, è necessario utilizzare una libreria esterna, come `regex` o `sregex`, che forniscono funzioni di manipolazione delle stringhe basate su espressioni regolari.

## Vedi anche:
- [Documentazione della libreria regex](https://docs.rs/regex/1.3.7/regex/)
- [Esempi di espressioni regolari in uso in Rust](https://github.com/rust-lang/regex/tree/master/examples)