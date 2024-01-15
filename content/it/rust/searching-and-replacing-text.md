---
title:                "Cercare e sostituire il testo"
html_title:           "Rust: Cercare e sostituire il testo"
simple_title:         "Cercare e sostituire il testo"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Ciao a tutti! Se siete qui, probabilmente siete alla ricerca di una soluzione per la ricerca e la sostituzione di testo nel vostro codice Rust. Potrebbe sembrare un compito noioso, ma la buona notizia è che Rust ha delle ottime funzionalità per gestire questa operazione in modo semplice e veloce.

## Come

Per iniziare, apriamo il nostro editor preferito e creiamo un nuovo file Rust. Diamo un'occhiata a due metodi diversi per effettuare la ricerca e la sostituzione di testo.

### Utilizzando la libreria regex

La prima opzione è utilizzare la libreria regex, che fornisce funzionalità avanzate per la manipolazione di espressioni regolari.

Includiamo la libreria all'inizio del nostro file con `use regex::Regex;`, creiamo una nuova istanza di Regex con il pattern che vogliamo cercare e poi utilizziamo la funzione `replace_all` per sostituire tutte le occorrenze della stringa di ricerca con la nuova stringa.

```Rust
use regex::Regex;

fn main() {
    let testo = "Questo è un esempio di testo in cui vogliamo sostituire una parola.";
    let regex = Regex::new("sostituire").unwrap();
    let nuovo_testo = regex.replace_all(testo, "cambiare");
    println!("{}", nuovo_testo);
}
```

L'output di questo codice sarà:

```
"Esempio di testo in cui vogliamo cambiare una parola."
```

### Utilizzando il metodo replace

Un altro modo per effettuare la sostituzione è utilizzare il metodo `replace` di Rust. Questo metodo accetta tre argomenti: la stringa di input, la stringa di ricerca e la stringa di sostituzione.

```Rust
fn main() {
    let testo = "Questo è un esempio di testo in cui vogliamo sostituire una parola.";
    let nuovo_testo = testo.replace("sostituire", "cambiare");
    println!("{}", nuovo_testo);
}
```

Anche in questo caso, l'output sarà:

```
"Esempio di testo in cui vogliamo cambiare una parola."
```

## Deep Dive

Ora che abbiamo visto due metodi diversi per effettuare la ricerca e la sostituzione di testo, vediamo alcune altre opzioni disponibili in Rust.

### Utilizzando un iteratore

Possiamo utilizzare un iteratore per applicare la sostituzione solo alle occorrenze specifiche che vogliamo. Ad esempio, possiamo sostituire solo la prima occorrenza del nostro pattern utilizzando il metodo `replace_first`.

```Rust
fn main() {
    let testo = "Questo è un esempio di testo in cui vogliamo sostituire una parola. Vogliamo sostituire solo la prima occorrenza.";
    let nuovo_testo = testo.replace_first("sostituire", "cambiare");
    println!("{}", nuovo_testo);
}
```

L'output sarà:

```
"Esempio di testo in cui vogliamo cambiare una parola. Vogliamo sostituire solo la prima occorrenza."
```

### Utilizzando un match pattern

Possiamo anche utilizzare un match pattern per sostituire una stringa solo se corrisponde a un determinato pattern. Ad esempio, se vogliamo sostituire solo le parole che iniziano con la lettera "s", possiamo utilizzare il seguente codice:

```Rust
fn main() {
    let testo = "Sostituiamo solo le parole che iniziano con la lettera s.";
    let nuovo_testo = testo.replace("sostituire", "cambiare");
    println!("{}", nuovo_testo);
}
```

L'output sarà:

```
"Cambiare solo le parole che iniziano con la lettera s."
```

## See Also

- Documentazione ufficiale di Rust su ricerca e sostituzione: https://doc.rust-lang.org/std/string/trait.Replace.html
- Tutorial su espressioni regolari in Rust: https://docs.rs/regex/1.3.6/regex/