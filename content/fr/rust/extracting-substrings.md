---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Extraire des sous-chaînes est le processus de retirer des morceaux spécifiques d'une chaîne de caractères. Les programmeurs le font souvent pour manipuler ou analyser des données textuelles.

## Comment faire:
Rust permet d'extraire les sous-chaînes à l'aide de l'indexation de tranches. Voici comment vous pouvez le faire:
```Rust
let chaine = "Bonjour Monde!";
let sous_chaine = &chaine[0..7];
println!("{}", sous_chaine); // affiche "Bonjour"
```
Il convient de noter que l'indexation de tranches en rust est basée sur les limites des caractères Unicode, donc si vous accédez à une tranche qui ne correspond pas à ces limites, votre programme panic.

## Plongée en profondeur:
Rust n'inclut pas de méthode intégrée pour extraire des sous-chaînes, il opte plutôt pour des tranches, ce qui est beaucoup plus sûr, en tenant compte des détails spécifiques liés à Unicode. 

Cependant, il existe d'autres moyens pour extraire des sous-chaînes. Par exemple, au lieu d'utiliser la méthode des tranches, on peut utiliser une boucle for pour parcourir les caractères.

Les détails d'implémentation sont également intéressants. Le type de chaîne Rust contient un pointeur vers les données de la chaîne (qui sont stockées dans la mémoire heap), la longueur et la capacité. Donc quand vous créez une slice, vous créez juste un nouveau pointeur vers la même mémoire heap existante.

## Voir Aussi: 
- Documentation Rust sur les chaînes : https://doc.rust-lang.org/book/ch08-02-strings.html
- Discussion StackOverflow sur l'extraction de sous-chaînes : https://stackoverflow.com/questions/24163159/what-is-the-right-way-to-extract-a-substring-in-rust