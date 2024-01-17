---
title:                "Imprimer la sortie de débogage"
html_title:           "Go: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi les programmeurs le font-ils?

La sortie de débogage est un moyen pour les programmeurs de suivre et de comprendre le fonctionnement de leur code. Cela implique de faire afficher des informations supplémentaires pendant l'exécution d'un programme, telles que des valeurs de variables ou des messages de débogage, afin de mieux comprendre ce qui se passe.

Les programmeurs utilisent souvent cette technique pour résoudre des problèmes et des bugs dans leur code, en identifiant les erreurs de logique ou les variables incorrectes. Cela leur permet également de vérifier si leur code fonctionne correctement et de le déboguer plus efficacement.

## Comment faire:

Voici un exemple de code en Go montrant comment ajouter un message de débogage dans un programme:

```
package main

import "fmt"

func main() {
    // Ajouter un message de débogage avec la fonction `Println` de la bibliothèque `fmt`
    fmt.Println("Débogage: Tentative de connexion au serveur...")

    // Exécuter le reste du code
    // ...
}
```

Voici un autre exemple où nous affichons la valeur d'une variable pendant l'exécution:

```
package main

import "fmt"

func main() {
    // Déclarer une variable `nom`
    nom := "Pierre"

    // Afficher la valeur de `nom` avec la fonction `Printf`
    fmt.Printf("Nom: %s", nom)

    // Exécuter le reste du code
    // ...
}
```

## Plongée en profondeur:

La sortie de débogage est une technique utilisée depuis longtemps par les programmeurs pour améliorer leur processus de débogage. Avant l'utilisation de cette technique, les programmeurs devaient souvent utiliser des méthodes de débogage plus manuelles, telles que l'impression de valeurs de variables dans la console ou l'utilisation de débogueurs externes.

Il existe également d'autres techniques de débogage en plus de la sortie de débogage, telles que le débogage par contrôle visuel, qui permet aux programmeurs de suivre l'exécution étape par étape et de visualiser les valeurs de variables en temps réel. Cependant, la sortie de débogage reste un outil efficace et populaire utilisé par de nombreux programmeurs.

En Go, la sortie de débogage peut être réalisée à l'aide de la bibliothèque standard `fmt` ou en utilisant des packages de débogage externes tels que `log` ou `debug`.

## Voir aussi:

- Documentation officielle de Go pour `fmt`: https://golang.org/pkg/fmt/
- Tutoriel sur la sortie de débogage en Go: https://gobyexample.com/debugging
- Utilisation du package `log` en Go: https://golang.org/pkg/log/