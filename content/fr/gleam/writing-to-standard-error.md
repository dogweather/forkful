---
title:                "Gleam: Ecrire vers l'erreur standard"
simple_title:         "Ecrire vers l'erreur standard"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être en train d'écrire du code en Gleam et vous vous demandez pourquoi vous devriez écrire à la sortie standard d'erreur (stderr). La réponse est simple : écrire à stderr est un moyen très utile de communiquer avec votre programmeur lors de l'exécution de votre code.

## Comment faire

Lorsque vous écrivez en Gleam, il est possible d'afficher des messagesà la sortie standard en utilisant la fonction ``stderr.write``. Voici un exemple de code pour vous montrer comment cela fonctionne :

```
Gleam stderr.write("Ceci est un message à la sortie standard")
```

Vous pouvez également inclure des variables dans vos messages à l'aide de ``stderr.format``. Voici un exemple :

```
Gleam stderr.format("L'utilisateur {} a tenté de se connecter", [username])
```

Lorsque votre code sera exécuté, les messages seront affichés à la sortie standard d'erreur, ce qui rendra le débogage et la compréhension de votre code beaucoup plus faciles.

## Plongée en profondeur

Écrire à la sortie standard d'erreur peut également être utile pour afficher des erreurs ou des avertissements lors de l'exécution de votre code. Vous pouvez également utiliser des bibliothèques externes comme ``gleam-pure`` pour gérer vos messages à la sortie standard de manière plus avancée.

En plus de ``stderr.write`` et ``stderr.format``, il existe également des fonctions telles que ``stderr.info``, ``stderr.warning``, et ``stderr.error`` pour vous aider à gérer différents types de messages à la sortie standard.

N'hésitez pas à explorer et expérimenter différentes façons d'utiliser l'écriture à la sortie standard d'erreur dans votre code Gleam.

## Voir aussi

- [Documentation officielle de Gleam sur l'écriture vers la sortie standard](https://gleam.run/documentation/stdlib/sys.stderr/)
- [Bibliothèque Gleam pour la gestion avancée de la sortie standard](https://github.com/gleam-lang/gleam-pure)
- [Exemples de code utilisant l'écriture vers la sortie standard en Gleam](https://github.com/search?q=gleam%2C+stderr&type=Code)