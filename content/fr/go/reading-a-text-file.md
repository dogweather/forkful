---
title:                "Lecture d'un fichier texte"
html_title:           "Go: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?

Lire un fichier texte est une tâche courante pour les programmeurs en Go. Cela signifie simplement ouvrir un fichier contenant du texte et le lire, ligne par ligne. Les programmeurs le font pour diverses raisons, comme lire des données stockées dans un format spécifique ou écrire des scripts pour traiter de grandes quantités de données.

# Comment Faire:

Voici un exemple de code simple pour lire un fichier texte en utilisant Go:

```
fichier, err := ouvrir ("chemin/vers/mon-fichier.txt")

if err ! = nil {
    fmt.Println (err)
    retourne
}

// boucle pour lire le fichier ligne par ligne
scanner := nouveau scanner (fichier)
pour scanner.Scan () {
    ligne := scanner.Text ()
    fmt.Println (ligne)
}

err = scanner.Err ()
if err ! = nil {
    fmt.Println (err)
}
```

Lorsque ce code est exécuté, chaque ligne du fichier texte sera imprimée à l'écran. Ce n'est qu'un exemple basique, mais il peut être adapté pour répondre aux besoins spécifiques de chaque programmeur.

# Plongée en Profondeur:

La lecture de fichiers texte est une tâche courante qui existe depuis longtemps dans le monde de la programmation. Avant l'avènement des ordinateurs, les programmeurs devaient utiliser des cartes perforées pour stocker leurs données, et les lire impliquait de passer chaque carte à travers une machine de lecture. Aujourd'hui, il existe d'autres alternatives à la lecture de fichiers texte, comme l'utilisation d'une base de données pour stocker et gérer les données. Cependant, la lecture de fichiers reste une méthode simple et efficace pour de nombreuses tâches de programmation.

En ce qui concerne l'implémentation de la lecture de fichiers en Go, il existe plusieurs packages disponibles, tels que "bufio" et "ioutil", qui offrent différentes méthodes pour lire des fichiers texte. Il est important de comprendre les différences entre ces packages et choisir celui qui convient le mieux à votre projet.

# Voir Aussi:

Pour en savoir plus sur la manipulation des fichiers en Go, voici quelques liens utiles:

- La documentation officielle du package "bufio": https://golang.org/pkg/bufio/
- La documentation officielle du package "ioutil": https://golang.org/pkg/io/ioutil/
- Un tutoriel pratique sur la lecture de fichiers en Go: https://www.javatpoint.com/golang-read-file