---
title:                "Écriture d'un fichier texte"
date:                  2024-01-19
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Écrire un fichier texte, c'est simplement sauver des données sous forme lisible. Les programmeurs le font pour la persistance des données, les logs, ou pour échanger des informations avec d'autres systèmes.

## Comment :

```go
package main

import (
    "bufio"
    "log"
    "os"
)

func main() {
    // Ouvrir ou créer le fichier
    fichier, err := os.Create("exemple.txt")
    if err != nil {
        log.Fatalf("Impossible de créer le fichier : %s", err)
    }
    defer fichier.Close()

    // Écrire dans le fichier avec bufio pour plus d'efficacité
    writer := bufio.NewWriter(fichier)
    _, err = writer.WriteString("Salut, comment ça va ?\n")
    if err != nil {
        log.Fatalf("Erreur lors de l'écriture dans le fichier : %s", err)
    }

    // S'assurer que toutes les opérations d'écriture sont bien effectuées
    writer.Flush()
}
```
Sortie attendue dans `exemple.txt` :
```
Salut, comment ça va ?
```

## Deep Dive

Avant Go, on écrivait des fichiers en C ou en Bash, moins sécurisé. Go offre `io/ioutil` et `os` pour simplifier le processus. L’utilisation de `bufio` est conseillée pour l'efficience. S'assurer d'appeler `defer Close()` pour éviter les fuites de mémoire.

## See Also

- Documentation officielle: [Package os](https://golang.org/pkg/os/)
- Article sur la manipulation des fichiers: [Using Go for File Handling](https://golangbot.com/read-files/)
- Tutoriel vidéo: [Writing Files in Go](https://www.youtube.com/watch?v=R0jVqeJ4FE0)
