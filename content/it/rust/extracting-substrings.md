---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cos’è & Perché?

L'estrazione di sottocordoni (substring) consiste nel prelevare un frammento di testo da una stringa più grande. I programmatori lo fanno per analizzare o manipolare specifici pezzi di dati da una fonte di input testuale.

## Come fare:

Per estrarre un sottocorda in Rust, utilizza l'indicizzazione sulla stringa come mostrato:

```Rust
let s = "Ciao, Mondo!";
let hello = &s[0..5];
let world = &s[7..13];
println!("{}, {}", hello, world); //Stampa: Ciao, Mondo
```
In questo esempio, abbiamo estratto "Ciao" e "Mondo" dalla stringa originale "Ciao, Mondo!" utilizzando gli indici per specificare le posizioni dei caratteri desiderati.

## Approfondimento:

L'estrazione di sottocordoni è un'operazione antica nella programmazione, iniziata con i linguaggi low-level come C. In Rust, a differenza di altri linguaggi, l'indicizzazione della stringa è basata su byte, non su caratteri. Questo può portare a errori se la stringa contiene caratteri multi-byte, come quelli in Unicode. Un'alternativa sarebbe l'uso del metodo `chars()` o `graphemes()` dalle librerie esterne.

Dettagli dell'implementazione:
Un `str` in Rust è una sequenza immutabile di codici UTF-8. Quando estraiamo una sottocorda, ciò che stiamo facendo realmente è creare un ‘puntatore’ al punto iniziale dell'intestazione e al punto finale nella stringa originale. Ciò significa che l'operazione è molto efficiente in termini di tempo e spazio, poiché non coinvolge la copia di dati.

## Vedi Anche:

Consulta i seguenti link per ulteriori informazioni e dettagli sulla gestione delle stringhe in Rust:
1. La documentazione ufficiale di Rust sulla [stringa](https://doc.rust-lang.org/book/ch08-02-strings.html) e [sottocorda](https://doc.rust-lang.org/std/string/struct.String.html#method.substring)
2. Discussione su [StackOverFlow](https://stackoverflow.com/questions/24151146/how-do-i-extract-a-substring-in-rust) sulla gestione delle stringhe in Rust.
3. Un tutorial su [Medium](https://medium.com/@carol.nichols/rusty-strings-2e9afd680914) sulla gestione delle stringhe in Rust.