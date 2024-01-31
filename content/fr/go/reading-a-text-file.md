---
title:                "Lecture d'un fichier texte"
date:                  2024-01-20T17:54:23.258963-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture d'un fichier texte"

category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Lire un fichier texte, c'est récupérer son contenu pour l'utiliser dans votre programme. Les développeurs font ça parce qu'ils ont besoin de données à traiter, stocker ou transmettre.

## How to:
```Go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
)

func main() {
    file, err := os.Open("exemple.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }
}
```
Sortie:
```
Première ligne du fichier.
Deuxième ligne du fichier.
Troisième ligne du fichier.
```

## Deep Dive
Historiquement, la lecture de fichiers suit un processus semblable dans de nombreux langages : ouverture, lecture ligne par ligne, puis fermeture du fichier. En Go, `bufio` et `ioutil` sont des packages souvent utilisés. Depuis Go 1.16, `ioutil` est déprécié et on recommande `os` et `io` à la place.

Il existe différentes méthodes pour lire un fichier en Go. `ioutil.ReadFile`, simplissime, lit tout le fichier d'un coup. `os.Open` avec `bufio.Scanner`, comme dans l'exemple, est plus adapté pour les gros fichiers, car il consomme moins de mémoire.

Les détails : `defer file.Close()` assure que le fichier sera fermé, évitant ainsi des fuites de ressources. `bufio.Scanner` lit le fichier ligne par ligne, ce qui est efficace pour parcourir de grands fichiers textes sans surcharger la mémoire.

## See Also
- Documentation Go sur les packages `bufio` et `os` : [https://pkg.go.dev/bufio](https://pkg.go.dev/bufio), [https://pkg.go.dev/os](https://pkg.go.dev/os)
