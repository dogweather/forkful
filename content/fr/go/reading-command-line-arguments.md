---
title:                "La lecture des arguments de ligne de commande"
html_title:           "Go: La lecture des arguments de ligne de commande"
simple_title:         "La lecture des arguments de ligne de commande"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Go, vous savez probablement déjà que les commandes de ligne de commande font partie intégrante du langage. Mais pourquoi devriez-vous vous intéresser à la lecture des arguments de ligne de commande? Eh bien, c'est parce que la capacité de lire et de traiter les arguments de ligne de commande peut être très utile lors de la création d'applications en ligne de commande ou de scripts.

## Comment faire

La lecture des arguments de ligne de commande en Go est très simple et intuitive. Tout d'abord, vous devez importer le package "os", qui fournit des fonctions pour récupérer les arguments de ligne de commande. Ensuite, vous pouvez utiliser la fonction "Args" de ce package pour obtenir une tranche contenant tous les arguments passés à l'application. Vous pouvez ensuite les traiter selon vos besoins.

Voici un exemple de code pour lire et imprimer les arguments de ligne de commande:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Obtient la tranche d'arguments de ligne de commande
	args := os.Args

	// Imprime tous les arguments
	fmt.Println(args)
}
```

Si vous exécutez ce code en passant quelques arguments comme ceci: "go run main.go arg1 arg2 arg3", vous obtiendrez la sortie suivante:

```shell
[main arg1 arg2 arg3]
```

Comme vous pouvez le voir, la tranche d'arguments de ligne de commande contient le nom du programme (dans cet exemple "main") suivi de tous les arguments passés. Vous pouvez ensuite utiliser ces arguments pour effectuer différentes actions dans votre application.

## Plongée en profondeur

Maintenant que vous savez comment lire les arguments de ligne de commande en Go, vous pouvez également approfondir vos connaissances en comprenant comment ces arguments sont structurés. Lorsque vous exécutez une application en ligne de commande en utilisant le package "os", le premier élément de la tranche des arguments sera toujours le nom du fichier exécutable. Ensuite, tous les arguments supplémentaires seront ajoutés à la tranche dans l'ordre dans lequel ils ont été passés.

De plus, vous pouvez également utiliser la fonction "Flag" du package "flag" pour déclarer des drapeaux dans votre application. Les drapeaux sont des arguments de ligne de commande avec des noms spécifiques, comme "--help" ou "--version". Vous pouvez ensuite utiliser ces drapeaux pour effectuer des actions spécifiques dans votre application.

## Voir aussi

Maintenant que vous connaissez les bases de la lecture des arguments de ligne de commande en Go, vous pouvez continuer à explorer d'autres fonctionnalités du langage telles que la manipulation de fichiers, les opérations réseau et bien plus encore. Voici quelques ressources utiles pour continuer votre apprentissage:

- [Documentation officielle de Go](https://golang.org/doc/)
- [Go by Example](https://gobyexample.com/command-line-arguments)
- [Cours en ligne interactif sur Go](https://www.codecademy.com/learn/learn-go)