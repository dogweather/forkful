---
title:    "Rust: Lecture des arguments en ligne de commande"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi
Avant de plonger dans les détails de la lecture des arguments de ligne de commande en Rust, il est important de comprendre pourquoi cette compétence est importante. En programmation, il est courant d'avoir besoin d'interagir avec un utilisateur en lui demandant des entrées ou des informations. La lecture des arguments de ligne de commande est une méthode efficace pour obtenir ces informations directement à partir de la ligne de commande.

## Comment Faire
Pour lire les arguments de ligne de commande en Rust, nous pouvons utiliser la fonction ```env::args()```, qui renvoie un itérateur qui parcourt les arguments fournis par l'utilisateur. Nous pouvons ensuite itérer sur cet itérateur pour traiter chaque argument individuellement.

Voici un exemple de code pour lire et afficher tous les arguments de ligne de commande :
```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    for arg in args {
        println!("{}", arg);
    }
}
```
Output:
```
$ cargo run arg1 arg2
arg1
arg2
```

## Plongée Profonde
La fonction ```env::args()``` renvoie également un premier argument qui correspond au chemin du binaire utilisé pour lancer le programme. Nous pouvons utiliser cette information pour ajouter de la logique supplémentaire à notre programme, par exemple pour afficher un message d'aide lorsque le programme est exécuté sans aucun argument.

De plus, il est important de noter que les arguments de ligne de commande sont fournis sous forme de chaînes de caractères (```String```). Si nous avons besoin de les convertir en un autre type de donnée, comme un nombre entier, nous devrons utiliser des fonctions de conversion appropriées, comme ```parse()```.

## Voir Aussi
- [Documentation officielle de Rust sur la lecture des arguments de ligne de commande](https://doc.rust-lang.org/stable/std/env/fn.args.html)
- [Exemple concret d'utilisation de la lecture des arguments de ligne de commande en Rust](https://www.educative.io/edpresso/how-to-read-command-line-arguments-in-rust)
- [Tutoriel vidéo sur la lecture des arguments de ligne de commande en Rust](https://www.youtube.com/watch?v=vRFP4X8TQr4)