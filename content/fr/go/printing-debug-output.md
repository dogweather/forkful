---
title:    "Go: Imprimer la sortie de débogage"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi
Le débogage est un élément essentiel du processus de développement en programmation. Que vous soyez un débutant ou un programmeur expérimenté, la capacité d'imprimer des informations de débogage peut vous faire gagner beaucoup de temps et faciliter le processus de recherche et de résolution des bugs dans votre code.

## Comment faire
En utilisant le langage de programmation Go, il est facile d'imprimer des informations de débogage dans votre code. Voici un exemple simple:

```Go
package main

import "fmt"

func main() {
    fmt.Println("Voici un exemple de message de débogage!")
}
```

Lorsque vous exécutez ce code, le message de débogage sera affiché dans la console de votre programme: "Voici un exemple de message de débogage!". Vous pouvez également utiliser la fonction fmt.Printf () pour formater et imprimer des données spécifiques telles que des variables et des valeurs booléennes.

## Plongée en profondeur
Il existe plusieurs techniques pour améliorer l'impression des informations de débogage. Par exemple, vous pouvez utiliser le package "log" qui vous permet de spécifier différents niveaux de messages de débogage tels que "info", "warning" et "error". Vous pouvez également utiliser la fonctionnalité de débogage intégrée de Go en ajoutant l'option de drapeau "-debug" lors de l'exécution de votre programme.

## Voir aussi
- [Guide de débogage officiel Go](https://golang.org/doc/gdb)
- [Article sur le débogage en Go](https://medium.com/swlh/debugging-in-go-mimsy-8e2144f8aef7)
- [Utiliser le package log en Go](https://golang.org/pkg/log/)