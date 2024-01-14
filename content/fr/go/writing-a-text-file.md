---
title:                "Go: Écrire un fichier texte"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire un fichier texte est un aspect essentiel de la programmation en Go. Cela permet de stocker des données de manière persistante et de les manipuler facilement dans le code. Dans cet article, nous allons découvrir pourquoi il est important de savoir écrire un fichier texte en Go.

## Comment faire

Pour créer un fichier texte en Go, nous allons utiliser le package `os` et sa fonction `Create`. Voici un exemple de code pour écrire un fichier texte avec le contenu "Bonjour, le monde !" :

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Ouverture du fichier en mode écriture, avec des permissions de 0666
    fichier, erreur := os.Create("hello.txt")
    // Vérifier s'il y a une erreur
    if erreur != nil {
        fmt.Println(erreur)
        return
    }
    // Écriture du contenu dans le fichier
    _, erreur = fichier.WriteString("Bonjour, le monde !")
    // Vérifier s'il y a une erreur
    if erreur != nil {
        fmt.Println(erreur)
        fichier.Close()
        return
    }
    // Fermrure du fichier
    fichier.Close()
}
```

En exécutant ce code, un nouveau fichier "hello.txt" sera créé avec le texte "Bonjour, le monde !" à l'intérieur.

## Plongée en profondeur

En Go, il est également possible de modifier un fichier texte existant ou d'en lire le contenu. Pour cela, nous allons utiliser les fonctions `Open` et `Read` ou `Write` du package `os`.

Par exemple, pour lire le contenu d'un fichier texte, nous pouvons utiliser le code suivant :

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Ouverture du fichier en mode lecture seulement
    fichier, erreur := os.Open("hello.txt")
    // Vérifier s'il y a une erreur
    if erreur != nil {
        fmt.Println(erreur)
        return
    }
    // Lecture du contenu du fichier en bytes
    bytes := make([]byte, 10)
    fichier.Read(bytes)
    // Conversion des bytes en string et affichage du contenu
    fmt.Println(string(bytes))
    // Fermeture du fichier
    fichier.Close()
}
```

Cela affichera "Bonjour, " car nous avons spécifié une longueur maximale de 10 bytes pour la lecture. Vous pouvez expérimenter avec différentes longueurs pour voir comment cela affecte le résultat final.

## Voir aussi

- [Documentation sur le package os en Go](https://pkg.go.dev/os)
- [Guide de référence pour la manipulation des fichiers en Go](https://gobyexample.com/writing-files)
- [Tutoriel vidéo sur l'écriture de fichiers en Go](https://www.youtube.com/watch?v=G4_9RpUi_N0)