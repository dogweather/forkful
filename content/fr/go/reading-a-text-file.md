---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?

Lire un fichier texte dans la programmation signifie extraire des informations à partir d'un fichier texte. Les programmeurs le font généralement pour analyser et manipuler des données sous forme de texte.

## Comment faire:

Voici un exemple de code qui montre comment lire un fichier texte en Go. Supposons que nous ayons un fichier texte appelé "example.txt".

```Go
package main

import (
    "bufio"
    "fmt"
    "os"
    "log"
)

func main() {
    file, err := os.Open("example.txt")
    if err != nil {
        log.Fatalf("failed to open file: %s", err)
    }

    scanner := bufio.NewScanner(file)
    scanner.Split(bufio.ScanLines)
    var txtlines []string

    for scanner.Scan() {
        txtlines = append(txtlines, scanner.Text())
    }

    file.Close()

    for _, eachline := range txtlines {
        fmt.Println(eachline)
    }
}
```
Ce script commencera par ouvrir le fichier 'example.txt'. Ensuite, il lira le fichier ligne par ligne et stockera chaque ligne dans le tableau 'txtlines'. Enfin, il fermera le fichier et affichera toutes les lignes du fichier à partir du tableau 'txtlines'.

## Plongée en profondeur:

Historiquement, la fonctionnalité de lecture de fichiers est l'une des plus anciennes dans le domaine de la programmation car elle est essentielle pour manipuler les données. En Go, le package `os` est couramment utilisé pour interagir avec le système de fichiers de l'ordinateur.

D'autres alternatives pour lire un fichier existent, comme ioutil.ReadFile dans le package ioutil, mais c'est moins efficace pour les gros fichiers texte car il lit tout le fichier en mémoire en une seul fois.

L’imbrication des fonctions Go os.Open, bufio.NewScanner et scanner.Scan fournit une méthode efficace pour lire séquentiellement de grands fichiers sans consommer beaucoup de mémoire.

## Voir aussi:

Les ressources suivantes fournissent des informations supplémentaires sur ce sujet:

1. Documentation officielle de Go sur le package 'os': https://golang.org/pkg/os/
2. Documentation officielle de Go sur le package 'bufio': https://golang.org/pkg/bufio/
3. Article de blog sur la lecture de fichiers en Go: https://www.devdungeon.com/content/working-files-go