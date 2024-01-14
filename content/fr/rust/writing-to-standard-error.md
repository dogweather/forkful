---
title:    "Rust: Écrire vers l'erreur standard"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire dans la sortie d'erreur standard est souvent nécessaire lorsque vous souhaitez afficher des messages d'erreur ou de débogage dans votre programme Rust. Cela peut être utile pour vous aider à identifier les problèmes dans votre code et à les corriger plus rapidement.

## Comment faire

Pour écrire dans la sortie d'erreur standard en Rust, vous pouvez utiliser la macro `eprint!` ou `eprintln!` selon que vous souhaitez afficher un message sans ou avec un retour à la ligne à la fin. Vous pouvez également utiliser le type `std::io::stderr` pour avoir un accès plus direct à la sortie d'erreur standard.

Voici un exemple de code utilisant la macro `eprintln!` pour afficher un message d'erreur :

```Rust
fn main() {
  let name = "Rust";
  eprintln!("Le langage {} est fantastique !", name);
}
```

Et voici le résultat de l'exécution de ce code :

```
Le langage Rust est fantastique !
```

## Plongée en profondeur

Il est important de noter que la sortie d'erreur standard est synchronisée, ce qui signifie que les messages seront affichés dans l'ordre dans lequel ils ont été appelés. De plus, si vous souhaitez afficher des messages d'erreur en utilisant la macro `eprint!` ou `eprintln!` dans une boucle, vous pouvez utiliser la fonction `std::io::Write::flush` pour forcer l'affichage immédiat du message dans la sortie d'erreur standard.

## Voir aussi

- [Document officiel Rust pour l'écriture sur la sortie d'erreur standard](https://doc.rust-lang.org/std/io/index.html#output-to-standard-error)
- [Guide de la communauté Rust sur l'écriture de messages d'erreur](https://rust-lang-nursery.github.io/api-guidelines/interoperability.html#error-messages)
- [Exemples pratiques d'utilisation de la sortie d'erreur standard en Rust](https://github.com/rust-lang/rust-by-example/blob/master/hello/print/print_debug.md)