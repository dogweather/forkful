---
title:                "Écrire vers l'erreur standard"
html_title:           "Rust: Écrire vers l'erreur standard"
simple_title:         "Écrire vers l'erreur standard"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur en herbe ou expérimenté, vous savez peut-être que la gestion des erreurs est une partie essentielle de tout programme. Lorsque quelque chose ne se passe pas comme prévu, il est important de pouvoir identifier précisément d'où vient le problème. C'est là que la sortie standard d'erreur (stderr) entre en jeu.

## Comment faire

Pour écrire vers la sortie standard d'erreur en Rust, nous utilisons la fonction `eprintln!()` qui fonctionne de la même manière que `println!()` pour la sortie standard (stdout), mais en écrivant vers stderr. Voici un exemple de son utilisation :

```Rust
fn main() {
    eprintln!("Ceci est un message d'erreur !");
}
```

Cela imprimera le message "Ceci est un message d'erreur !" dans la sortie standard d'erreur. Il est important de noter que la sortie standard d'erreur est différente de la sortie standard et elle est généralement affichée en rouge dans le terminal pour la différencier.

## Plongée en profondeur

La sortie standard d'erreur est un moyen utile de gérer les erreurs, mais comment peut-elle être utilisée de manière plus avancée ? Il existe différentes façons de contrôler la sortie standard d'erreur en Rust. Par exemple, vous pouvez utiliser une macro personnalisée pour écrire directement vers la sortie standard d'erreur sans avoir à taper `eprintln!()` à chaque fois. Vous pouvez également rediriger la sortie standard d'erreur vers un fichier au lieu du terminal.

## Voir aussi

Pour en savoir plus sur la gestion des erreurs en Rust, voici quelques ressources utiles :

- [Documentation officielle de Rust sur la gestion des erreurs](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
- [Article sur Medium expliquant l'utilisation de la sortie standard d'erreur en Rust](https://medium.com/@izgzhen/utilizing-stderr-in-rust-programming-language-34951588c9a9)
- [Exemple pratique de gestion des erreurs en Rust avec la bibliothèque "thiserror"](https://dev.to/nithanaroy/using-thiserror-to-improve-the-error-handling-of-your-rust-programs-391p)

Maintenant que vous avez une meilleure compréhension de l'utilisation de la sortie standard d'erreur en Rust, n'hésitez pas à l'implémenter dans vos prochains projets pour une meilleure gestion des erreurs !