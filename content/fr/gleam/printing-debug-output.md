---
title:    "Gleam: Afficher la sortie de débogage"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez probablement pourquoi imprimer des messages de débogage serait utile en programmation Gleam. Eh bien, cette pratique est très courante dans le développement logiciel car elle permet de vérifier l'état d'exécution d'un programme et de détecter des erreurs potentielles.

## Comment faire

Pour imprimer des messages de débogage dans votre code Gleam, vous pouvez utiliser la fonction `debug` de la bibliothèque standard `gleam/debug`. Voici un exemple de code pour imprimer un message d'erreur avec cette fonction :

```Gleam
import gleam/debug

fn validate_name(name) {
  if name == "" {
    debug.error("Le nom ne peut pas être une chaîne vide.")
  }
}
```

Lorsque ce code est exécuté, le message "Le nom ne peut pas être une chaîne vide." sera imprimé dans la console.

## Plongée en profondeur

Il est important de noter que la fonction `debug` peut prendre une liste d'arguments supplémentaires en plus du message. Cela vous permet de fournir des informations précieuses sur l'état de votre programme lors de l'impression de messages de débogage, tels que des valeurs de variables, des paramètres de fonction ou des résultats de calcul.

De plus, la fonction `debug` n'imprime des messages que si votre application est exécutée en mode "debug", ce qui est déterminé par la valeur du paramètre `GLEAM_ENV` dans votre fichier `gleam.toml`.

## Voir aussi

- [Documentation officielle de la fonction `debug` de Gleam](https://gleam.run/stdlib/debug.html)
- [Tutoriel sur l'utilisation de messages de débogage en Gleam](https://medium.com/gleam-lang/utiliser-des-messages-de-d%C3%A9bogage-en-gleam-6c40be61a485)