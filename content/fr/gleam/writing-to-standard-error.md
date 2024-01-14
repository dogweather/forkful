---
title:                "Gleam: Écrire dans le flux d'erreur standard"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi écrire sur l'erreur standard en Gleam ?

L'erreur standard est un outil important pour les programmeurs en Gleam car elle leur permet d'afficher des messages d'erreur et de déboguer leur code plus efficacement. Sans cette fonctionnalité, il serait plus difficile de traquer et de résoudre les problèmes dans un programme.

## Comment faire ?

Pour écrire sur l'erreur standard en Gleam, utilisez la fonction `io.stderr()` suivie du message que vous souhaitez afficher. Voici un exemple de code :

```
Gleam io.stderr("Erreur : imprimé sur l'erreur standard")
```

Lorsque vous exécutez ce code, le message sera imprimé sur l'erreur standard, ce qui pourrait être utile pour signaler une erreur ou pour afficher des informations de débogage.

## Plongée en profondeur

L'erreur standard en Gleam est une entrée de sortie spéciale qui permet d'envoyer des messages d'erreur ou de débogage lors de l'exécution d'un programme. Cela peut être particulièrement utile pour les programmes en mode console ou pour les tests unitaires. Il est également possible d'écrire sur l'erreur standard à partir d'un gestionnaire de processus en utilisant la fonction `Process.write_to_stderr()`.

# Voir aussi

- [Documentation Gleam sur l'erreur standard](https://gleam.run/articles/io.html#stderr)
- [Article sur les meilleures pratiques de débogage en Gleam](https://tpolecat.github.io/2018/03/25/gentle-intro-to-gleam.html#debugging)
- [Exemples de code utilisant l'erreur standard en Gleam](https://github.com/gleam-lang/gleam/tree/main/examples)