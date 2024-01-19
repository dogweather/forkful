---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce et pourquoi?
Les arguments de ligne de commande sont des valeurs qui sont passées à un programme lors de son exécution. Les programmeurs les utilisent pour contrôler le comportement des applications sans avoir à changer le code.

## Comment faire:
Lisons les commandes en Go. Voici un exemple simple:
```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  for idx, val := range os.Args {
    fmt.Printf("Argument %d: %s\n", idx, val)
  }
}
```
Lorsqu’on exécute ce programme avec des arguments, cela donne:
```Shell
$ go run main.go arg1 arg2 arg3
Argument 0: /tmp/go-build405710029/b001/exe/main
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```
On peut voir que `os.Args` contient tous les arguments, y compris le nom du programme lui-même.

## Pénétration Profonde
Historiquement, la lecture des arguments de ligne de commande est un aspect clé des applications UNIX - où tout est axé sur la ligne de commande. Les alternatives à `os.Args` pourraient être l'utilisation de `flag` ou `os.Args` de Go qui fournissent plus de contrôle et de flexibilité. En termes d'implémentation, `os.Args` est un slice de chaînes, où chaque élément est un argument commandé. Le premier (index 0) est le nom du programme lui-même.

## Voir aussi
Pour plus d'informations, consulter la documentation officielle Go sur les arguments de ligne de commande: [os package](https://golang.org/pkg/os/), [flag package](https://golang.org/pkg/flag/). Voir aussi un tutoriel intéressant sur [Go by Example](https://gobyexample.com/command-line-arguments).