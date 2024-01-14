---
title:    "Go: Lecture d'un fichier texte"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire et manipuler des fichiers texte est une tâche courante pour les programmeurs en Go. Cela peut être utile pour extraire des données, les traiter ou les utiliser dans d'autres parties de votre code. Dans cet article, nous allons découvrir comment lire un fichier texte en utilisant Go et pourquoi c'est important.

## Comment faire

Pour lire un fichier texte en Go, nous allons utiliser la fonction `ReadFile` de la bibliothèque` io/ioutil`. Elle prend en paramètre le chemin du fichier à lire et renvoie le contenu du fichier sous forme de tableau de bytes. Voyons un exemple de code pour mieux comprendre :

```Go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    // Chemin du fichier à lire
    chemin := "exemple.txt"

    // Lecteur de fichier
    contenu, err := ioutil.ReadFile(chemin)

    if err != nil {
        fmt.Println(err)
    }

    // Afficher le contenu du fichier
    fmt.Println(string(contenu))
}
```

Dans cet exemple, nous importons les packages nécessaires pour lire un fichier et utilisons la fonction `ReadFile`. Nous utilisons ensuite la fonction `string` pour convertir le tableau de bytes en chaîne de caractères afin de l'afficher.

## Plongée en profondeur

Lire un fichier texte peut sembler simple, mais il y a quelques points à garder à l'esprit. Tout d'abord, il est important de gérer les erreurs en utilisant une structure `if` pour capturer les éventuelles erreurs lors de la lecture du fichier. Deuxièmement, il est important d'utiliser la fonction `string` pour convertir le tableau de bytes en chaîne de caractères afin de pouvoir manipuler les données plus facilement.

Il est également possible de lire un fichier ligne par ligne en utilisant la fonction `ReadLine` de la bibliothèque `bufio`. Enfin, n'oubliez pas de fermer le fichier après avoir terminé de le lire en utilisant la fonction `Close`.

## Voir aussi

- [Documentation officielle pour la lecture de fichiers en Go](https://golang.org/pkg/io/ioutil/#ReadFile)
- [Exemple de lecture de fichier ligne par ligne en Go](https://www.ardanlabs.com/blog/2014/12/reading-files-in-go.html)
- [Guide complet pour la création et la manipulation de fichiers en Go](https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-go)

Maintenant que vous savez comment lire un fichier texte en utilisant Go, vous pouvez l'appliquer à votre propre code pour manipuler des données et réaliser de nombreuses tâches utiles ! N'hésitez pas à explorer d'autres fonctionnalités et options disponibles pour lire des fichiers en Go. Bonne programmation !