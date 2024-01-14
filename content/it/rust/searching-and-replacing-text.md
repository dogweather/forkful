---
title:                "Rust: Sostituzione e ricerca di testo"
simple_title:         "Sostituzione e ricerca di testo"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

La ricerca e la sostituzione di testo è un'operazione comune quando si lavora con programmi informatici. Con Rust, un linguaggio di programmazione moderno e sicuro, è possibile eseguire facilmente questa operazione in modo efficiente e sicuro.

## Come fare

Per eseguire la ricerca e la sostituzione di testo in Rust, è necessario utilizzare il metodo `replace` della libreria standard `String`:

```
Rust let frase = "Ciao amici, benvenuti nel mio blog!";
let nuova_frase = frase.replace("amici", "lettori");
```

In questo esempio, stiamo sostituendo la parola "amici" con "lettori" nella stringa `frase`. Il risultato ottenuto sarà "Ciao lettori, benvenuti nel mio blog!".

Inoltre, è possibile specificare una keyword "n" per sostituire solo la prima n occorrenza della parola cercata:

```
Rust let parola = "Rust è fantastico e potente";
let nuova_parola = parola.replace("fantastico", "eccezionale", 1);
```

In questo caso, stiamo sostituendo solo la prima occorrenza della parola "fantastico" con "eccezionale". Il risultato sarà "Rust è eccezionale e potente".

## Approfondimento

Per effettuare una ricerca e sostituzione di testo più avanzata in Rust, è possibile utilizzare l'API Regex. Con l'uso delle espressioni regolari, è possibile specificare pattern più complessi da cercare e sostituire.

Ad esempio, il codice seguente sostituisce tutte le lettere minuscole con lettere maiuscole in una stringa:

```
Rust use regex::Regex;

let regex = Regex::new(r"[a-z]").unwrap();
let testo = "ciao a tutti!";
let risultato = regex.replace_all(testo, |caps| {
    caps[0].to_uppercase()
});

```

Il risultato sarà "CIAO A TUTTI!"

## Vedi anche

- [Documentazione della libreria standard di Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [API Regex per Rust](https://docs.rs/regex/1.4.5/regex/)