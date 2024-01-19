---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Créer un fichier temporaire est l'action de constituer un fichier pour usage à court terme ou en essai. Les programmeurs font cela pour tester des fonctionnalités sans endommager les fichiers permanents ou pour manipuler d'énormes quantités de données sans consommer la mémoire principale.

## Comment faire :

Voici un exemple de code en Go pour créer un fichier temporaire.

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    tempFile, err := ioutil.TempFile(".", "tempFile-*.txt")
    if err != nil {
        fmt.Println(err)
        os.Exit(1)
    }
    defer os.Remove(tempFile.Name())

    fmt.Println("Fichier temporaire créé :", tempFile.Name())
}
```

Dans ce code, nous utilisons la fonction `ioutil.TempFile` pour créer un fichier temporaire. En exécutant ce code, vous devriez voir un message comme celui-ci :

```
Fichier temporaire créé : ./tempFile-123456.txt
```

## Plongée profonde:

Historiquement, les fichiers temporaires sont des éléments ancestraux de la programmation car le stockage sur disque était autrefois beaucoup plus abondant que la mémoire vive. De nos jours, ils restent essentiels pour gérer de grandes quantités de données.

Un autre avantage est de servir de tampon entre les opérations de lecture / écriture sur le disque et la mémoire, augmentant ainsi l'efficacité de vos programmes.

Au lieu de `ioutil.TempFile`, vous pourriez aussi utiliser `os.CreateTemp`, une fonction introduite dans Go 1.16. Toutefois, l'ioutil.TempFile` est plus couramment utilisé dans les versions antérieures.

Voici comment créer un fichier temporaire avec `os.CreateTemp` :

```Go
tempFile, err := os.CreateTemp(".", "tempFile-*.txt")
```

## Voir aussi :

Pour plus d'informations, consultez [la documentation officielle de ioutil.TempFile](https://pkg.go.dev/io/ioutil#TempFile) et [la documentation de os.CreateTemp](https://pkg.go.dev/os#CreateTemp). 

N'oubliez pas non plus de consulter [cette ressource utile](https://go.dev/blog/defer-panic-and-recover) sur l'utilisation correcte de `defer` pour nettoyer les fichiers temporaires.