---
title:                "Majuscule d'une chaîne de caractères"
html_title:           "Go: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Capitaliser une chaîne de caractères signifie mettre en majuscule la première lettre de chaque mot dans une phrase. Les programmeurs le font souvent pour rendre les textes plus lisibles et pour respecter les conventions de codage.

## Comment le faire:
```Go
package main

import "fmt"
import "strings"

func capitalize(str string) string {
	words := strings.Split(str, " ")

	for i, word := range words {
		words[i] = strings.Title(word)
	}

	return strings.Join(words, " ")
}

func main() {
	fmt.Println(capitalize("hello world"))
}

```

Output: "Hello World"

## Plongée en profondeur:
Historiquement, la capitalisation a été utilisée pour faciliter la lecture des textes imprimés en majuscules uniquement. De nos jours, elle est principalement utilisée dans le domaine de la programmation pour améliorer la lisibilité du code. D'autres alternatives existent, comme l'utilisation de majuscules uniquement pour les constantes en Go. L'implémentation de la fonction de capitalisation variera selon le langage de programmation utilisé.

## Voir aussi:
- [Documentation officielle de Go sur les chaînes de caractères](https://golang.org/doc/effective_go.html#strings)
- [Alternative à la capitalisation: utilisation de majuscules pour les constantes en Go](https://blog.golang.org/constants)
- [Article sur l'importance de la lisibilité du code en programmation](https://blog.codinghorror.com/code-readability/)