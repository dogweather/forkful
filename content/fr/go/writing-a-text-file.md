---
title:    "Go: Écriture d'un fichier texte"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi
Pourquoi écrire un fichier texte en programmation Go ? Il y a plusieurs raisons possibles, notamment pour stocker des données ou pour créer un rapport ou un document. Dans cet article, nous allons explorer comment écrire un fichier texte en utilisant Go.

## Comment Faire
Pour écrire un fichier texte en Go, nous allons d'abord ouvrir un fichier en utilisant la fonction `os.Create()` qui prend en paramètre le nom du fichier que nous voulons créer. Ensuite, nous allons utiliser la méthode `WriteString()` pour écrire le contenu du fichier. Enfin, nous allons fermer le fichier en utilisant la méthode `Close()`. Voici un exemple de code :

```Go
fichier, err := os.Create("exemple.txt")

if err != nil {
    fmt.Println(err)
    return
}

defer fichier.Close()

_, err = fichier.WriteString("Ceci est un exemple de texte écrit en Go.")
if err != nil {
    fmt.Println(err)
    return
}
```
Le résultat de ce code sera un fichier texte nommé "exemple.txt" contenant la phrase "Ceci est un exemple de texte écrit en Go.". 

## Plongée en Profondeur
Il est important de noter que pour écrire correctement un fichier texte, il est nécessaire de gérer les erreurs potentielles en utilisant des conditions `if` et des variables `err`. Nous pouvons également utiliser la méthode `Write()` pour écrire du contenu binaire dans un fichier texte. De plus, il est possible d'utiliser la méthode `Append()` pour ajouter du contenu à un fichier déjà existant.

## Voir Aussi
- Guide de référence pour les fichiers en Go : https://golang.org/pkg/os/
- Tutoriel pour écrire des fichiers en Go : https://tutorialedge.net/golang/reading-writing-files-in-go/
- Blog sur l'utilité des fichiers en programmation : https://www.codementor.io/@kaushikpal882/how-to-use-text-files-for-data-persistence-go-r1syi2gbp