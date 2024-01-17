---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Rust: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi nous le faisons?

Lecture des arguments de ligne de commande, c'est le fait de récupérer les informations fournies par l'utilisateur lorsque le programme est appelé à partir de la ligne de commande. Les programmeurs le font pour rendre leurs programmes plus flexibles et personnalisables, en leur permettant d'être contrôlés par les utilisateurs via la ligne de commande.

## Comment faire:

Voici un exemple de code en Rust pour lire les arguments de ligne de commande:
```Rust
use std::env;

fn main() {
    // Récupérer les arguments
    let args: Vec<String> = env::args().collect();
    
    // Afficher le premier argument (le nom du programme)
    println!("Nom du programme: {}", args[0]);
    
    // Afficher le deuxième argument (premier argument fourni par l'utilisateur)
    println!("Premier argument: {}", args[1]);
    
    // Afficher tous les arguments (à l'exception du premier qui est le nom du programme)
    println!("Tous les arguments: {:?}", &args[1..]);
}
```

En exécutant ce code avec la commande `rustc main.rs` puis `./main hello world`, nous obtenons la sortie suivante:
```
Nom du programme: ./main
Premier argument: hello
Tous les arguments: ["hello", "world"]
```

## Plongée en profondeur:

La lecture des arguments de ligne de commande est une pratique courante dans la programmation, et est souvent utilisée pour contrôler le comportement du programme ou pour fournir des paramètres. D'autres langages de programmation comme C et Python ont également des moyens pour lire les arguments de ligne de commande.

Une alternative à la lecture des arguments de ligne de commande en Rust est d'utiliser une librairie externe, comme `clap` ou `structopt`, qui offrent plus de fonctionnalités et une meilleure gestion des erreurs.

Au niveau de l'implémentation, la fonction `env::args()` renvoie un itérateur sur les arguments, qui peut être facilement converti en une `Vec` ou un autre type de collection. La documentation officielle de Rust offre plus de détails sur cette fonction.

## Voir aussi:

Vous pouvez consulter la documentation officielle de Rust sur `env::args()` pour en apprendre davantage sur la lecture des arguments de ligne de commande en Rust. Vous pouvez également jeter un coup d'œil à la librairie `clap` ou `structopt` pour des alternatives plus avancées.