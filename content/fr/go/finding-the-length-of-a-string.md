---
title:                "Go: Trouver la longueur d'une chaîne"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi 

La recherche de la longueur d'une chaîne peut sembler simple et sans intérêt à première vue. Mais en réalité, c'est une tâche fondamentale en programmation. Savoir comment trouver la longueur d'une chaîne peut être très utile dans de nombreux scénarios de programmation, tels que la manipulation de données et la vérification de la validité des entrées utilisateur.

## Comment faire 

Pour trouver la longueur d'une chaîne en Go, vous pouvez utiliser la fonction `len()`. Elle prend en paramètre la chaîne que vous souhaitez évaluer et retourne le nombre de caractères dans cette chaîne.

```
Go 

package main 

import "fmt" 

func main() { 
    str := "Bonjour tout le monde" 
    fmt.Println(len(str)) 
} 

// Output: 20
```

Comme on peut le voir dans l'exemple ci-dessus, la fonction `len()` peut être utilisée sur n'importe quelle chaîne de caractères. Elle est également utile pour vérifier si une chaîne est vide ou non. Si `len()` retourne 0, cela signifie que la chaîne est vide.

Vous pouvez également utiliser la méthode `String()` pour convertir un autre type de donnée en une chaîne de caractères, puis utiliser la fonction `len()` pour trouver sa longueur.

```
Go 

package main 

import ( 
    "fmt" 
    "strconv" 
) 

func main() { 
    num := 1234 
    str := strconv.Itoa(num) 
    fmt.Println(len(str)) 
} 

// Output: 4
```

## Deep Dive 

La fonction `len()` en Go est basée sur le concept de runes. Une rune est une valeur entière 32 bits qui représente un caractère Unicode. Cela signifie que la longueur retournée par `len()` correspond au nombre de runes dans une chaîne, et non au nombre de caractères visibles.

Par exemple, si nous prenons la phrase "Bonjour tout le monde" et la convertissons en Unicode, elle aura une longueur de 20 runes car elle contient des caractères spéciaux tels que "é". Mais si nous la convertissons en ASCII, sa longueur serait de 18, car l'ASCII ne prend pas en compte les caractères spéciaux.

Il est également important de noter que `len()` ne peut pas être utilisé sur des types de données autres que les chaînes de caractères en Go. Si vous essayez de l'utiliser sur une liste ou un tableau, vous obtiendrez une erreur.

## Voir aussi 

- [La documentation officielle sur la fonction `len()` en Go](https://golang.org/pkg/builtin/#len)
- [Un tutoriel sur les runes et les chaînes Unicode en Go](https://golangbot.com/strings/)
- [Un article sur les différences entre l'Unicode et l'ASCII](https://www.beyondjava.net/about-unicode-75-characters)