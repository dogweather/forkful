---
title:                "La lecture d'un fichier texte"
html_title:           "Go: La lecture d'un fichier texte"
simple_title:         "La lecture d'un fichier texte"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi
Peut-être que vous vous demandez pourquoi vous devriez prendre le temps de lire un fichier texte en utilisant Go. La réponse est simple : la lecture de fichiers texte est une tâche courante dans de nombreux projets de programmation, que ce soit pour récupérer des données ou pour les traiter. En apprenant à le faire en Go, vous pourrez élargir vos compétences en programmation et utiliser un langage performant pour cette tâche.

## Comment faire
Voici un exemple de code en Go qui montre comment lire un fichier texte et afficher son contenu :

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Ouverture du fichier en mode lecture seule
    file, err := os.Open("monfichier.txt")
    if err != nil {
        // En cas d'erreur, affichage du message et arrêt du programme
        fmt.Println("Erreur lors de l'ouverture du fichier :", err)
        return
    }
    defer file.Close() // Fermeture du fichier à la fin de la fonction

    // Lecture du contenu du fichier
    buffer := make([]byte, 100) // Buffer pour stocker les données lues
    for {
        // Lecture des données dans le buffer
        n, err := file.Read(buffer)

        // Si on atteint la fin du fichier, on sort de la boucle
        if err == io.EOF {
            break
        }

        // Affichage du contenu du buffer en tant que chaîne de caractères
        fmt.Println(string(buffer[:n]))
    }
}
```
Supposons que le contenu de "monfichier.txt" soit "Bonjour le monde !". La sortie de ce programme sera :

```
Bonjour le monde !
```

## Plongée en profondeur
En utilisant la méthode `Read()` de l'objet `File`, on peut lire des données à partir d'un fichier en utilisant un buffer. La méthode renvoie le nombre d'octets lus et le cas échéant, une éventuelle erreur. En utilisant une boucle, on peut lire le contenu d'un fichier en plusieurs fois si celui-ci est trop grand pour tenir dans le buffer en une seule fois. Il est également important de fermer le fichier à la fin de la fonction pour libérer les ressources utilisées.

## Voir aussi
- Documentation officielle sur la lecture de fichiers en Go : https://golang.org/pkg/os/#File.Read
- Tutoriel sur la lecture et l'écriture de fichiers en Go : https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-go
- Démo en ligne de lecture de fichiers en Go : https://play.golang.org/p/S5HJsPXJF8c