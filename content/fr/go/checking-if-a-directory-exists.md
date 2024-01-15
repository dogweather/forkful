---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Go: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous avez besoin de vérifier régulièrement si un dossier existe dans votre projet Go et vous voulez savoir comment le faire efficacement. Cela peut vous aider à gérer les fichiers et les ressources de manière plus dynamique dans votre code.

## Comment faire

Voici un exemple de code pour vérifier si un dossier existe en utilisant la fonction `os.Stat()` :

```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  folderPath := "chemin/vers/mon/dossier"

  // Vérification de la statut du dossier
  if _, err := os.Stat(folderPath); os.IsNotExist(err) {
    fmt.Println("Le dossier n'existe pas.")
  } else {
    fmt.Println("Le dossier existe.")
  }
}
```

Le code ci-dessus utilise la fonction `os.Stat()` pour obtenir la statut d'un dossier spécifique, puis utilise la fonction `os.IsNotExist()` pour vérifier si le dossier n'existe pas. La fonction `IsNotExist()` renvoie un booléen indiquant si le dossier existe ou non.

Voici un exemple de sortie si le dossier existe :

```
Le dossier existe.
```

Et voici un exemple si le dossier n'existe pas :

```
Le dossier n'existe pas.
```

## Plongée en profondeur

La fonction `os.Stat()` renvoie une erreur si le dossier n'existe pas ou si l'accès est refusé. Vous pouvez également utiliser la fonction `os.IsPermission()` pour vérifier si l'erreur est due à un problème de permissions d'accès.

De plus, si vous avez besoin de créer un dossier si celui-ci n'existe pas encore, vous pouvez utiliser la fonction `os.Mkdir()` pour le créer. Voici un exemple de code qui utilise à la fois la vérification de l'existence du dossier et la création si nécessaire :

```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  folderPath := "chemin/vers/mon/dossier"

  // Vérification de la statut du dossier
  if _, err := os.Stat(folderPath); os.IsNotExist(err) {

    // Création du dossier
    err := os.Mkdir(folderPath, 0755)
    if err != nil {
      fmt.Println("Erreur lors de la création du dossier :", err)
      return
    }
    fmt.Println("Le dossier a été créé.")
  }
}
```

## Voir aussi

- [Documentation officielle de la fonction os.Stat()](https://pkg.go.dev/os#Stat)
- [Documentation officielle de la fonction os.Mkdir()](https://pkg.go.dev/os#Mkdir)
- [Article sur la gestion de fichiers avec Go](https://www.digitalocean.com/community/tutorials/how-to-use-the-filesystem-package-in-go-fr)