---
title:                "Rust: Ricerca e sostituzione di testo"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Perché

Il testo è un elemento fondamentale nella programmazione, ed è spesso necessario modificarlo per adattarlo alle esigenze del nostro codice. La Rust offre strumenti potenti per la ricerca e la sostituzione del testo, rendendo questo compito semplice ed efficiente.

# Come fare

Per eseguire una ricerca e sostituzione del testo in Rust, possiamo utilizzare il metodo `replace()` della struttura `String`. Questo metodo accetta due parametri: il testo da cercare e il testo con cui sostituirlo. Ecco un esempio di come utilizzarlo:

```Rust
let text = "Questo è un esempio di testo.";
let new_text = text.replace("esempio", "esempio modificato");
println!("{}", new_text);
```

Questo codice stamperà "Questo è un esempio modificato di testo." come output. Possiamo anche utilizzare il metodo `replace()` su una `String` immutabile, in questo caso verrà restituita una nuova `String` con il testo modificato.

È inoltre possibile utilizzare il metodo `replace()` con espressioni regolari per eseguire ricerche più avanzate. Per farlo, dobbiamo importare il modulo `regex` e utilizzare il metodo `replace_all()` invece di `replace()`:

```Rust
use regex::Regex;

let text = "Una chiave non è solo una chiave.";
let re = Regex::new("chiave").unwrap();
let new_text = re.replace_all(text, "parola");
println!("{}", new_text);
```

Questo codice stamperà "Una parola non è solo una parola." come output, poiché il metodo `replace_all()` sostituisce tutte le occorrenze del testo che corrispondono all'espressione regolare.

# Approfondimento

Il metodo `replace()` della struttura `String` implementa il trait `Replace`, che è anche implementato per altri tipi come `&str` e `&mut str`. Ciò significa che possiamo utilizzare il metodo `replace()` su qualunque tipo che implementi questo trait, semplificando il nostro codice e rendendolo più flessibile.

Un altro approfondimento interessante è l'utilizzo di iteratori e il metodo `fold()` della struttura `String` per eseguire ricerche e sostituzioni su grandi quantità di testo in modo efficiente.

# Vedi anche

- Documentazione ufficiale della Rust sul metodo `replace()`: https://doc.rust-lang.org/std/string/struct.String.html#method.replace
- Documentazione ufficiale della Rust sul modulo `regex`: https://doc.rust-lang.org/regex/regex/index.html
- Guida introduttiva alla regex in Rust: https://rust-lang-nursery.github.io/rust-cookbook/text/regex.html