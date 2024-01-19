---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La longueur d'une chaîne est le nombre de caractères qu'elle contient. Les programmeurs la trouvent souvent pour gérer les limites de saisie, découper les chaînes, valider le format, etc.

## Comment faire :
Voici comment trouver la longueur d'une chaîne en Go:

```Go
package main

import "fmt"

func main() {
    str := "Bonjour, Go!"
    fmt.Println(len(str))
}
```

Aussi simple qu'il y paraît, ce code imprime `12`, qui est le nombre de caractères dans `"Bonjour, Go!"`.

## Plongeon en profondeur
Go utilise UTF-8 pour les chaînes, donc chaque "caractère" peut prendre 1 à 4 octets. La fonction `len()` donne le nombre d'octets, pas toujours le nombre de caractères. Par exemple :

```Go
package main

import "fmt"

func main() {
    str := "Café"
    fmt.Println(len(str)) // Affiche 5, pas 4!
}
```

Ici, `"Café"` a 4 caractères, mais `len()` donne `5`. Pourquoi? Parce que `len()` compte les octets, et `é` est un caractère UTF-8 de 2 octets.

Si vous voulez le nombre de caractères (rune en Go), utilisez `utf8.RuneCountInString()`. Par exemple :

```Go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "Café"
    fmt.Println(utf8.RuneCountInString(str)) // Affiche 4
}
```

## Voir aussi
* Documentation officielle de Goodoc sur les chaînes: [Strings](https://golang.org/pkg/strings/)
* UTF-8 et Go: [Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)
* Go par l'exemple: [String Functions](https://gobyexample.com/string-functions)