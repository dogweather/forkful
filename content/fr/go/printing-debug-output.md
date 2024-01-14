---
title:                "Go: Affichage des résultats de débogage"
simple_title:         "Affichage des résultats de débogage"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Imaginons que vous êtes en train d'écrire un programme en Go et que vous rencontrez des problèmes avec certaines parties de votre code. Vous savez que quelque chose ne fonctionne pas correctement, mais vous n'arrivez pas à comprendre où se situe exactement l'erreur. C'est là que l'impression de la sortie de débogage peut être utile.

## Comment faire

La méthode la plus simple pour imprimer des informations de débogage est d'utiliser la fonction `fmt.Println()`. Voici un exemple de code qui illustre son utilisation :

```Go
package main

import "fmt"

func main() {
    name := "Jean"
    age := 28

    fmt.Println("Mon nom est", name, "et j'ai", age, "ans.")
}
```

Le résultat de ce code sera :

```
Mon nom est Jean et j'ai 28 ans.
```

Il est également possible d'imprimer des informations plus détaillées à l'aide de la fonction `fmt.Printf()`, qui utilise des directives de format pour spécifier le type de données à imprimer. Voici un exemple :

```Go
package main

import "fmt"

func main() {
    name := "Marie"
    age := 32

    fmt.Printf("Mon nom est %s et j'ai %d ans.", name, age)
}
```

Et le résultat sera le même que le précédent :

```
Mon nom est Marie et j'ai 32 ans.
```

## Plongée en profondeur

Il existe de nombreuses autres fonctions utiles pour l'impression de la sortie de débogage en Go, telles que `fmt.Sprintf()` pour stocker les résultats dans une variable, `log.Print()` pour l'impression de journaux ou encore `panic()` pour interrompre le programme en cas d'erreur grave.

Il est également important de savoir que l'impression de la sortie de débogage ne doit être utilisée que pendant le processus de développement et ne devrait pas être présente dans le code final, car cela peut avoir un impact sur les performances et la lisibilité du code.

## Voir aussi

- [Documentation de la fonction fmt.Println() en français](https://golang.org/pkg/fmt/#Println)
- [Guide de débogage en Go](https://blog.golang.org/debugging-go-code)
- [Débogage avec Visual Studio Code](https://code.visualstudio.com/docs/go/debugging)