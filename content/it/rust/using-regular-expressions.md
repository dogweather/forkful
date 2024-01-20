---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Le espressioni regolari (regex) permettono di cercare e manipolare testo basandosi su pattern definiti. I programmatori le usano per validare input, estrarre dati, e semplicemente cercare stringhe di testo in modo potente e flessibile.

## How to:
Per usare regex in Rust, bisogna usare il crate `regex`. Installalo aggiungendo `regex = "1.5.4"` nelle dipendenze del tuo `Cargo.toml`.

```Rust
use regex::Regex;

fn main() {
    let testo = "Il 2023 sarà ricco di sorprese!";
    let re = Regex::new(r"\d+").unwrap();
    for numero in re.find_iter(testo) {
        println!("{}", numero.as_str());
    }
}
```

Output:
```
2023
```

## Deep Dive
Le regex sono nate negli anni '50 e continuano ad evolversi. Rust utilizza la libreria `regex`, scritta in Rust, nota per essere sicura e veloce. Alternative includono pattern matching nativo di Rust, ma regex offre più potenza per testi complessi. La 'lazy compilation' dei pattern e l'uso efficiente della memoria sono aspetti chiave dell'implementazione Rust.

## See Also
- [La documentazione ufficiale del crate `regex`](https://docs.rs/regex/1.5.4/regex/)
- [Tutorial sul pattern matching in Rust](https://doc.rust-lang.org/book/ch18-00-patterns.html)