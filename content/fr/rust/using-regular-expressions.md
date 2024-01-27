---
title:                "Utilisation des expressions régulières"
date:                  2024-01-19
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Les expressions régulières (regex), c'est filtrer du texte avec précision. On les utilise pour chercher, valider ou remplacer du texte par motifs.

## How to:
Rust incorpore `regex` via une crate externe. Voici comment s'en servir :

```rust
use regex::Regex;

fn main() {
    let texte = "Le numéro de Rust est 1.58.1";
    let regex = Regex::new(r"\d+\.\d+\.\d+").unwrap();

    match regex.find(texte) {
        Some(matched) => println!("Version trouvée : {}", matched.as_str()),
        None => println!("Aucune version détectée."),
    }
}
```

Sortie:
```
Version trouvée : 1.58.1
```

## Deep Dive
Les regex en Rust sont basées sur la librairie `regex` écrite en Rust. Avant ça, on utilisait souvent des libs en C. Alternatives? `grep` en shell, ou `str` méthodes de Rust. Les regex de Rust sont rapides et évitent les problèmes de sécurité communs.

## See Also
- La documentation officielle [`regex` crate](https://docs.rs/regex)
- [Syntaxe des expressions régulières](https://www.regular-expressions.info/)
- Le chapitre sur les expressions régulières dans [The Rust Programming Language](https://doc.rust-lang.org/book/ch08-02-strings.html#searching-for-patterns-in-strings)
