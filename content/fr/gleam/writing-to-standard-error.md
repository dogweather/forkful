---
title:                "Écrire dans le canal d'erreur standard"
html_title:           "Gleam: Écrire dans le canal d'erreur standard"
simple_title:         "Écrire dans le canal d'erreur standard"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire vers l'erreur standard peut sembler être une tâche inutile, mais c'est en fait une étape cruciale dans le processus de débogage de votre code. L'affichage des messages d'erreur sur la sortie standard peut permettre de mieux comprendre les problèmes et de les résoudre plus rapidement.

## Comment faire

Pour écrire vers l'erreur standard en Gleam, vous allez utiliser la fonction `write_stderr` qui accepte une chaîne de caractères en tant que paramètre. Elle va ensuite afficher cette chaîne de caractères dans la sortie d'erreur standard. Voici un exemple de comment utiliser cette fonction :

```
Gleam

fn main() {
  write_stderr("Erreur : Cette fonction n'est pas encore implémentée");
}
```

Output :

```
Erreur : Cette fonction n'est pas encore implémentée
```

## Plongée en profondeur

Ecrire vers l'erreur standard peut être utile lorsque votre code rencontre une erreur et que vous souhaitez en avertir l'utilisateur. Cela peut également être utile pour déboguer votre code en affichant des informations supplémentaires sur l'état de votre programme.

Il est important de n'utiliser l'écriture vers l'erreur standard que pour les informations de débogage et de ne pas en faire une méthode de gestion des erreurs dans votre programme. Au lieu de cela, vous devriez utiliser des types d'erreurs et des gestionnaires d'erreurs pour un traitement optimal des erreurs dans votre code.

## Voir aussi

- [Documentation de Gleam sur l'écriture vers l'erreur standard](https://gleam.run/book/tour/writing_to_stderr.html)
- [Article sur la gestion des erreurs en Gleam](https://gleam.run/articles/handling_errors.html)