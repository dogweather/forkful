---
title:                "Go: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

#Pourquoi

Les arguments de ligne de commande sont souvent utilisés dans les programmes pour fournir des paramètres personnalisés au moment de l'exécution. Apprendre à lire et à utiliser ces arguments peut vous aider à créer des programmes plus dynamiques et personnalisables en Go.

#Comment Faire

Pour lire les arguments de ligne de commande en Go, nous utilisons la fonction `os.Args` qui renvoie une slice (tranche) contenant les arguments passés lors de l'exécution du programme. Par exemple, si vous avez un programme appelé "monprogramme.go" et que vous l'exécutez avec l'argument "bonjour", la fonction `os.Args` renverra une slice contenant `["monprogramme.go", "bonjour"]`. Voici un exemple de code pour lire et afficher les arguments de ligne de commande :

```
package main

import (
    "fmt"
    "os"
)

func main() {
    args := os.Args

    fmt.Println("Voici les arguments de ligne de commande :")
    for _, arg := range args {
        fmt.Println(arg)
    }
}
```

Lorsque vous exécutez ce programme avec différents arguments, vous obtiendrez des résultats différents :

```
$ go run monprogramme.go bonjour
Voici les arguments de ligne de commande :
monprogramme.go
bonjour
```

```
$ go run monprogramme.go salut tout le monde
Voici les arguments de ligne de commande :
monprogramme.go
salut
tout
le
monde
```

#Plongée en Profondeur

La fonction `os.Args` ne fait que renvoyer les arguments sous forme de slice, mais il y a plusieurs façons de les utiliser dans votre code Go. Par exemple, vous pouvez utiliser un `switch` pour vérifier si certains arguments ont été passés lors de l'exécution du programme et agir en conséquence. Vous pouvez également utiliser la librairie `flag` pour définir et utiliser des drapeaux (flags) pour les arguments de ligne de commande.

Il est également important de noter que les arguments de ligne de commande doivent être passés dans l'ordre dans lequel votre programme les attend. Par exemple, si vous utilisez `os.Args[2]` pour accéder au troisième argument, mais que celui-ci n'est pas passé lors de l'exécution du programme, vous obtiendrez une erreur.

#Voir Aussi

- [Documentation officielle Go - Arguments de ligne de commande](https://golang.org/pkg/os/#Args)
- [Tutoriel vidéo sur les arguments de ligne de commande en Go](https://www.youtube.com/watch?v=p5aCpo4ZTcE)
- [Utiliser la librairie flag en Go](https://gobyexample.com/command-line-flags)