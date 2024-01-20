---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi?

Imprimer des traces de débogage, c'est afficher des informations qui aident à comprendre le déroulement et l'état d'un programme. Les programmeurs l'utilisent souvent pour dépister et résoudre les bugs, valider le comportement attendu du logiciel.

## Comment faire :

Vous pouvez implémenter la capacité de débogage dans Rust avec le macro dbg!. Ainsi, il affiche une trace de l'expression et de sa valeur. Voici un exemple basique:

```Rust
fn main() {
    let l = (1..5).collect::<Vec<_>>();
    dbg!(l);
}
```

Ce code générera l'affichage suivant :

```Rust
[src/main.rs:3] l = [
   1,
   2,
   3,
   4
]
```

## Plongée en profondeur:

En Rust, le debug a toujours été au cœur de la conception. Historiquement, l'équipe de développement de Rust a mis l'accent sur l'importance du débogage pratique et informatif, ce qui a conduit à l'introduction du macro dbg!.

Des alternatives existent, comme le module `log` qui fournit un ensemble de macros pour le journalisation, du niveau d'erreur à celui d'information. Cependant, pour le débogage simple, dbg! est souvent plus pratique.

L'implémentation courante de '`dbg!`’ forme une chaîne de caractères contenant le nom du fichier source, le numéro de ligne et le résultat de l'expression. L'expression est évaluée une seule fois.

## Pour en savoir plus:

Pour plus d'informations sur le débogage avec Rust, consultez les liens suivants :

- Documentation officielle de Rust : https://doc.rust-lang.org/std/macro.dbg.html
- Guide de débogage de Rust : https://github.com/rust-lang/rls/blob/master/debugging.md