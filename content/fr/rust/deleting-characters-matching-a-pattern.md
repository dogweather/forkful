---
title:                "Suppression de caractères correspondant à un motif"
date:                  2024-01-20T17:43:02.555874-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suppression de caractères correspondant à un motif"

category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
La suppression de caractères correspondant à un motif, c’est l’acte d’enlever des caractères d’une chaîne de caractères (string) qui matchent un certain pattern. On fait ça pour nettoyer des données, valider des entrées ou simplifier le traitement de textes.

## How to:
Utilisons `regex` pour matcher et supprimer des patterns spécifiques. 

```Rust
extern crate regex;
use regex::Regex;

fn main() {
    let text = "Les dates: 2023-03-17, 2023-04-01";
    let date_re = Regex::new(r"\d{4}-\d{2}-\d{2}").unwrap();
    let result = date_re.replace_all(text, "");
    println!("Texte après suppression: '{}'", result);
}
```

Output:
```
Texte après suppression: 'Les dates: , '
```

## Deep Dive
Historiquement, les expressions régulières viennent de la théorie des automates et des langages formels. En Rust, pour manipuler des regex, on utilise souvent la crate `regex`, qui est performante et bien intégrée.

Dans nos alternatives, on a `str::replace`, pratique pour des cas simples, ou en utilisant `Vec<char>` pour de l'itération plus contrôlée. Les détails d'implémentation chez `regex` incluent des compilations de patterns en automates finis pour une recherche efficace.

## See Also

- [Rust documentation on Strings](https://doc.rust-lang.org/book/ch08-02-strings.html): Pour une meilleure compréhension des strings en Rust.
- [Automata Theory](https://en.wikipedia.org/wiki/Automata_theory): Pour comprendre les bases théoriques derrière les regex.
