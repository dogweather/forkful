---
title:    "Gleam: Écrire vers l'erreur standard"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi
L'utilisation de la sortie standard est essentielle pour le débogage de programmes. Il vous permet d'afficher des messages d'erreur et des informations supplémentaires qui peuvent vous aider à comprendre ce qui se passe dans votre code.

## Comment faire
Pour écrire dans la sortie standard en Gleam, utilisez la fonction `io.stderr.write` suivie du message que vous souhaitez afficher. Voici un exemple de code avec une sortie d'erreur personnalisée :

```Gleam
fn main() {
  let error_message = get_error_message()
  io.stderr.write(error_message)
}

fn get_error_message() {
  let message = "Une erreur est survenue."
  message
}
```

Cela va générer la sortie suivante dans le terminal :

```
Une erreur est survenue.
```

Vous pouvez également inclure des variables et des opérations dans votre message d'erreur en les concaténant avec `++`. Par exemple :

```Gleam
let account_balance = 1000
let error_message = "Solde insuffisant. Vous avez " ++ account_balance ++ " dollars sur votre compte."
```
La sortie serait alors :

```
Solde insuffisant. Vous avez 1000 dollars sur votre compte.
```

## Deep Dive
Il est important de noter que la sortie standard n'est pas seulement utile pour afficher des erreurs, mais aussi pour déboguer des programmes en général. Vous pouvez l'utiliser pour afficher des variables, des valeurs de retour de fonctions ou toute autre information pertinente à des fins de débogage.

De plus, la bibliothèque standard de Gleam offre d'autres fonctions pour écrire dans la sortie standard, telles que `io.stderr.write_line` pour inclure un saut de ligne à la fin de votre message.

## Voir aussi
- [Documentation officielle de Gleam sur la sortie standard](https://gleam.run/docs/std/io#stderr)
- [Article de blog sur le débogage avec la sortie standard en Rust](https://medium.com/swlh/debugging-with-stderr-in-rust-292c0f35648b)
- [Tutoriel sur les bases de Gleam](https://eli-gleam.github.io/tut-fr/)