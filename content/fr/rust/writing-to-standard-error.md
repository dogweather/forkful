---
title:                "Rust: Ecrire dans la sortie d'erreur standard"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi écrire vers l'erreur standard en Rust

Si vous êtes développeur en Rust, il est très probable que vous ayez entendu parler de l'erreur standard ou STDERR. Mais pourquoi et quand devriez-vous envisager d'écrire vers l'erreur standard dans vos programmes ? Dans cet article, nous allons plonger dans les raisons pour lesquelles cette pratique peut être utile et comment le faire efficacement.

## Comment Faire

Ecrire vers l'erreur standard en Rust est un processus assez simple. Tout d'abord, vous devez importer le module `std::io` qui contient des fonctionnalités pour écrire vers l'erreur standard. Ensuite, vous pouvez utiliser la fonction `io::stderr` pour obtenir un objet représentant l'erreur standard. Vous pouvez ensuite appeler la fonction `write` sur cet objet pour écrire vers l'erreur standard. Voici un exemple de code :

```Rust
use std::io;

fn main() {
    let mut stderr = io::stderr();
    stderr.write(b"Erreur standard : Ce n'est pas un entier !\n").unwrap();
}
```

Dans cet exemple, nous utilisons la méthode `write` pour écrire une chaîne de caractères vers l'erreur standard en utilisant `b` pour indiquer qu'il s'agit d'un littéral de type byte. N'oubliez pas d'appeler la méthode `unwrap` pour gérer les erreurs potentielles.

Si vous souhaitez écrire vers l'erreur standard en utilisant des formattages de type `println!`, vous pouvez utiliser la macro `eprintln!` qui fonctionne de la même manière que `println!` mais écrit vers l'erreur standard. Voici un autre exemple :

```Rust
fn main() {
    let age = 25;
    eprintln!("Erreur standard : l'âge doit être un entier, {} n'est pas un entier valide.", age);
}
```

Vous pouvez également utiliser la fonction `eprint!` pour écrire sans saut de ligne à la fin.

## Plongée en profondeur

Maintenant que nous savons comment écrire vers l'erreur standard en Rust, explorons pourquoi c'est une bonne pratique. L'erreur standard est un flux de sortie qui est toujours disponible, contrairement à la sortie standard (STDOUT) qui peut être redirigée ou supprimée. Cela en fait un bon choix pour écrire des informations importantes ou des erreurs lors de l'exécution d'un programme.

De plus, écrire vers l'erreur standard est également utile lorsque vous travaillez avec des processus en arrière-plan ou en réseau. Au lieu d'afficher des messages sur la console, vous pouvez utiliser l'erreur standard pour les enregistrer dans un fichier ou les envoyer à un autre ordinateur.

## Voir Aussi

- [Documentation de la bibliothèque standard Rust : Module std::io](https://doc.rust-lang.org/std/io/index.html)
- [Article sur l'écriture vers l'erreur standard en Rust](https://crates.io/crates/stderrlog)
- [Article sur les flux d'entrée/sortie en Rust](https://levans.fr/rust-io-streams-input-output.html)