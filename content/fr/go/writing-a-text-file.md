---
title:                "Écrire un fichier texte"
html_title:           "Go: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?
Écrire un fichier texte en programmation consiste simplement à enregistrer des données sous forme de texte brut dans un fichier. Les programmeurs le font pour stocker des informations de manière structurée et facilement accessible pour leur programme.

# Comment:
```
Go package main

import (
    "fmt"
    "os"
)

func main() {
    file, err := os.Create("text_file.txt") // crée un nouveau fichier texte
    if err != nil {
        fmt.Println(err)
        return
    }
    defer file.Close() // ferme le fichier une fois le programme terminé

    file.WriteString("Ceci est un exemple de données à enregistrer dans notre fichier texte") // écrit des données dans le fichier

    fmt.Println("Le fichier texte a été créé et les données ont été enregistrées avec succès")
}
```

Sortie:
```
Le fichier texte a été créé et les données ont été enregistrées avec succès
```

# Exploration Approfondie:
La création de fichiers texte est une pratique courante en programmation car elle offre un moyen simple de stocker et d'organiser des données. Cela peut être utile pour stocker des informations de configuration, des journaux, des données de base de données ou tout autre type de données. Il existe également des alternatives à l'écriture de fichiers texte, telles que l'utilisation de bases de données, mais c'est souvent une solution plus complexe. En termes d'implémentation, des langages de programmation tels que Go offrent des fonctions et des méthodes pour faciliter la création et l'écriture de fichiers texte.

# Voir aussi:
- Documentation officielle de Go sur l'écriture de fichiers: https://golang.org/pkg/os/#Create
- Tutoriel sur l'écriture et la lecture de fichiers en Go: https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-go