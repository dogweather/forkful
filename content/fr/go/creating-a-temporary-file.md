---
title:    "Go: Créer un fichier temporaire"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en programmation Go ?

Créer un fichier temporaire est utile lorsqu'on a besoin d'une zone de stockage temporaire pour les données dans un programme Go. Cela peut être utile notamment pour gérer des données temporaires, comme lors du traitement de fichiers volumineux ou lors de la communication avec des serveurs externes.

## Comment créer un fichier temporaire en programmation Go ?

Il existe plusieurs façons de créer un fichier temporaire en Go, en voici une :

```
package main

import (
    "io/ioutil"
    "log"
    "os"
)

func main() {
    // Création d'un fichier temporaire avec un préfixe et une extension
    tempFile, err := ioutil.TempFile("", "example*.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer os.Remove(tempFile.Name()) // Suppression du fichier temporaire après utilisation
    // Exemple d'écriture et de lecture de données dans le fichier temporaire
    _, err = tempFile.Write([]byte("Contenu temporaire"))
    if err != nil {
        log.Fatal(err)
    }
    fileContent, err := ioutil.ReadFile(tempFile.Name())
    if err != nil {
        log.Fatal(err)
    }
    log.Printf("Le contenu du fichier temporaire est : %s", fileContent)
}
```

La sortie de ce code sera :

```
2021/10/06 18:00:00 Le contenu du fichier temporaire est : Contenu temporaire
```

## Plongez plus en profondeur dans la création de fichiers temporaires en Go

Pour ceux qui sont intéressés à en savoir plus sur la création de fichiers temporaires en Go, voici quelques éléments à prendre en compte :

- Par défaut, les fichiers temporaires seront créés dans le répertoire de temporaire système de l'utilisateur. Cela peut être modifié en spécifiant un chemin dans le premier argument de `ioutil.TempFile()`.
- Les fichiers temporaires générés auront un nom unique avec un suffixe aléatoire, mais il est possible de spécifier un préfixe ou une extension en fonction de ses besoins.
- Il est important de supprimer les fichiers temporaires après utilisation pour éviter de saturer le disque avec des fichiers inutiles.

## Voir aussi

- [Documentation officielle de Go sur les fichiers temporaires](https://golang.org/pkg/io/ioutil/#TempFile)
- [Exemples de création de fichiers temporaires en Go](https://gobyexample.com/temporary-files)