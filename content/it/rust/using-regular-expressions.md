---
title:                "Rust: L'utilizzo delle espressioni regolari"
simple_title:         "L'utilizzo delle espressioni regolari"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché
RegEx, abbreviazione di "Regular Expressions", è una potente e flessibile tecnica di ricerca e manipolazione di testo. Nel mondo della programmazione, le espressioni regolari sono uno strumento fondamentale per la gestione dei dati. Imparare a utilizzarle può sembrare un po' intimidatorio, ma una volta compresa la loro logica e sintassi, si apriranno infinite possibilità per la gestione dei dati.

## Come fare
```Rust
use regex::Regex;

fn main() {
    // Creazione di una nuova espressione regolare
    let re = Regex::new(r"Hello, \w+!").unwrap();

    // Esecuzione della ricerca su una stringa
    let text = "Hello, world!";
    assert!(re.is_match(text));

    // Stampa del risultato
    println!("La stringa \"{}\" combacia con l'espressione regolare \"Hello, \\w+!\"", text);
}
```

Ecco un semplice esempio di utilizzo delle espressioni regolari in Rust. Innanzitutto, si importa il pacchetto regex e si crea una nuova espressione regolare utilizzando il metodo `Regex::new`. La stringa passata come argomento rappresenta il pattern che vogliamo cercare. In questo caso, cerchiamo una stringa che inizi con "Hello," seguito da uno o più caratteri alfanumerici (`\w` in regex indica tutti i caratteri alfanumerici, mentre il `+` rappresenta uno o più occorrenze).  Successivamente, eseguiamo la ricerca utilizzando il metodo `is_match` sulla stringa desiderata. Infine, stampiamo il risultato con `println`.

## Approfondimenti
Le espressioni regolari sono molto più che un semplice metodo per cercare stringhe. Possono essere utilizzate per separare e sostituire parti di stringhe, validare formati di dati e molto altro. La loro logica di base è basata su una combinazione di caratteri speciali e simboli che permettono di creare pattern complessi per la ricerca dei dati desiderati. Per saperne di più sull'utilizzo avanzato delle espressioni regolari in Rust, è possibile consultare la documentazione ufficiale su [regex-rs](https://docs.rs/regex/). Inoltre, è possibile praticare le proprie conoscenze utilizzando [regex101](https://regex101.com/), un'ottima risorsa online per testare ed esplorare espressioni regolari.

## Vedi anche
- [Documentazione ufficiale di regex-rs](https://docs.rs/regex/)
- [Regex101 - tool per testare e esplorare espressioni regolari](https://regex101.com/)