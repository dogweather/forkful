---
title:    "Go: Vérification de l'existence d'un répertoire"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Pourquoi

Vérifier si un répertoire existe peut sembler être une tâche simple, mais cela peut être une étape cruciale dans la création d'applications robustes en Go. Cela peut vous aider à gérer les erreurs et à vous assurer que votre code s'exécute correctement sans rencontrer des problèmes de non-existence de répertoires.

## Comment faire

Pour vérifier si un répertoire existe en utilisant Go, vous pouvez utiliser la fonction `os.Stat()` et vérifier si l'erreur renvoyée est `nil` ou non. Voici un exemple de code avec une sortie de démonstration :

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Définition du chemin du répertoire à vérifier
    dirPath := "./mon_repertoire"

    // Utilisation de la fonction os.Stat() pour vérifier si le répertoire existe
    _, err := os.Stat(dirPath)

    if os.IsNotExist(err) {
        // Si l'erreur renvoyée est os.IsNotExist, alors le répertoire n'existe pas
        fmt.Println("Le répertoire n'existe pas.")
    } else {
        // Sinon, le répertoire existe
        fmt.Println("Le répertoire existe.")
    }
}

```

La sortie du code ci-dessus dépendra de l'existence ou non du répertoire spécifié. Voici un exemple de sortie pour un répertoire existant :

```
Le répertoire existe.
```

Et voici un exemple de sortie pour un répertoire inexistant :

```
Le répertoire n'existe pas.
```

## Plongée en profondeur

En plongeant en profondeur, il est important de noter que la fonction `os.Stat()` renverra une erreur `nil` même si le répertoire spécifié existe mais que l'utilisateur n'a pas les droits d'accès nécessaires. Si vous souhaitez vous assurer que le répertoire est à la fois existant et que vous avez les droits d'accès nécessaires, vous pouvez utiliser la fonction `os.Stat()` suivie de la fonction `os.IsPermission()` pour vérifier si l'erreur renvoyée est due à une autorisation insuffisante.

## Voir également

- La documentation officielle de la fonction `os.Stat()` en Go : https://golang.org/pkg/os/#Stat
- Un exemple pratique de vérification de la présence d'un répertoire en utilisant Go : https://www.digitalocean.com/community/tutorials/how-to-use-the-new-go-error-handling-features-fr