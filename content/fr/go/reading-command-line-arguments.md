---
title:                "Go: Lecture des arguments en ligne de commande"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Les arguments de ligne de commande sont un aspect essentiel de la programmation en Go. Ils permettent à un programme de recevoir des données d'entrée de la part de l'utilisateur, ce qui rend l'interaction avec le programme plus dynamique et personnalisable.

## Comment faire

Pour lire les arguments de ligne de commande en Go, il faut d'abord importer le package `os`, qui contient la fonction `Args` pour récupérer les arguments. Ensuite, il suffit d'utiliser une boucle `for` pour parcourir tous les arguments et les traiter selon les besoins du programme.

Voici un exemple de code pour lire et afficher les arguments de ligne de commande :

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args

	for i, arg := range args {
		fmt.Printf("Argument %d : %s\n", i, arg)
	}
}
```

Si on exécute ce code avec la commande `go run main.go hello world`, on obtient la sortie suivante :

```
Argument 0: main
Argument 1: hello
Argument 2: world
```

## Plongée en profondeur

Pour aller plus loin, il est possible de trier ou de filtrer les arguments de ligne de commande selon certains critères, en utilisant les fonctions et les variables du package `flag` ou en faisant des manipulations basées sur des expressions régulières.

Il est également important de noter que le premier argument (index 0) est généralement le nom du programme lui-même, et que les arguments sont toujours de type `string`.

Maintenant que vous savez comment lire et traiter les arguments de ligne de commande en Go, vous pouvez rendre vos programmes plus interactifs et plus flexibles !

## Voir aussi

- [Documentation officielle sur les arguments de ligne de commande en Go](https://golang.org/pkg/os/#pkg-variables)
- [Article sur le package `flag` en Go](https://golang.org/pkg/flag/)
- [Tutoriel sur les expressions régulières en Go](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go-fr)