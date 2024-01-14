---
title:    "Go: La lecture des arguments de ligne de commande"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Pourquoi

Les arguments de ligne de commande sont un aspect essentiel du développement en programmation. Ils permettent aux utilisateurs d'interagir avec votre programme en fournissant des inputs spécifiques lors de son exécution. Dans cet article, nous allons voir comment lire ces arguments en utilisant le langage de programmation Go.

## Comment faire

Pour lire les arguments de ligne de commande en Go, nous avons besoin d'utiliser le package "os". Ce package fournit des fonctions pour interagir avec le système d'exploitation, y compris pour récupérer les arguments de ligne de commande. Voici un exemple de code qui affiche les arguments fournis par l'utilisateur :

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // os.Args est un tableau qui contient tous les arguments de la ligne de commande
    args := os.Args
    // Pour chaque argument, nous l'imprimons sur une nouvelle ligne
    for _, arg := range args {
        fmt.Println(arg)
    }
}
```

Si vous exécutez ce code avec les arguments "go" et "programmation", vous obtiendrez en sortie :

```
go
programmation
```

## Approfondissement

Maintenant que nous savons comment récupérer les arguments de ligne de commande, voyons ce que nous pouvons en faire. Nous pouvons utiliser ces arguments pour personnaliser le comportement de notre programme en fonction des inputs fournis par l'utilisateur. Par exemple, si notre programme est un convertisseur de devises, l'utilisateur peut spécifier les montants et les taux de change en utilisant les arguments.

Nous pouvons également utiliser des packages tels que "flag" pour créer des options de ligne de commande plus avancées, comme les drapeaux de type booléen ou les valeurs par défaut.

## Voir aussi

- [Documentation officielle de Go sur os package](https://golang.org/pkg/os/)
- [Tutoriel sur les arguments de ligne de commande en Go](https://gobyexample.com/command-line-arguments)
- [Package flag pour une utilisation plus avancée des arguments de ligne de commande en Go](https://golang.org/pkg/flag/)