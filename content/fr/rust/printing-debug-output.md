---
title:                "Affichage de sortie de débogage"
html_title:           "Rust: Affichage de sortie de débogage"
simple_title:         "Affichage de sortie de débogage"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Imprimer des sorties de débogage est une technique qui permet aux programmeurs de voir des informations utiles pendant l'exécution d'un programme. Cela peut les aider à identifier et à résoudre les erreurs plus facilement.

## Comment faire :

Utilisez la macro ``eprintln!`` pour imprimer une sortie de débogage sur la sortie d'erreur standard :

```
fn main() {
    let a = 5;
    eprintln!("La valeur de a est {}", a);
}
```

Cela produira une sortie de débogage comme celle-ci :

```
La valeur de a est 5
```

Vous pouvez également utiliser la macro ``dbg!`` pour faire la même chose, mais cette fois-ci, la sortie de débogage sera imprimée sur la sortie standard :

```
fn main() {
    let a = 5;
    dbg!(a);
}
```

Résultat :

```
[src/main.rs:3] a = 5
```

## Plongée en profondeur :

Avant la version 1.32 de Rust, il n'y avait pas de solution standard pour imprimer des sorties de débogage. Les programmeurs devaient donc utiliser des astuces telles que les macros personnalisées ou les fonctions spéciales pour y parvenir.

La macro ``dbg!`` a été ajoutée dans la version 1.32, facilitant ainsi l'impression de sorties de débogage en utilisant une syntaxe plus simple et plus intuitive. De plus, la macro ``eprintln!`` est également disponible pour ceux qui préfèrent imprimer leurs sorties de débogage sur la sortie d'erreur standard.

Il existe également d'autres alternatives pour imprimer des sorties de débogage en Rust, telles que la bibliothèque ``log`` qui propose des fonctionnalités avancées pour gérer les niveaux de débogage et la journalisation.

En termes d'implémentation, la macro ``dbg!`` utilise la fonction interne ``format!`` pour formater la sortie de débogage. Quant à la macro ``eprintln!``, elle utilise la fonction interne ``eprint!`` pour imprimer sur la sortie d'erreur standard.

## Voir aussi :

- Guide officiel de Rust sur la sortie de débogage : https://doc.rust-lang.org/rust-by-example/hello/print/print_debug.html
- Bibliothèque ``log`` pour la gestion des niveaux de débogage : https://docs.rs/log/0.4.14/log/