---
title:    "Go: Mettre en majuscule une chaîne de caractères"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation en Go, il est souvent nécessaire de manipuler des chaînes de caractères. L'une des opérations courantes est la capitalisation d'une chaîne, c'est-à-dire rendre la première lettre en majuscule. Dans cet article, nous allons découvrir pourquoi et comment capitaliser une chaîne en Go.

## Comment Faire

La capitalisation d'une chaîne en Go se fait en utilisant la fonction `strings.ToTitle()` du package `strings`. Cette fonction prend en paramètre une chaîne de caractères et retourne la même chaîne avec la première lettre en majuscule.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "bonjour"
	capitalized := strings.ToTitle(str)
	fmt.Println(capitalized)
}

// Output: Bonjour
```

Il est également possible de capitaliser uniquement la première lettre d'une chaîne en utilisant la fonction `strings.Title()`. Cette dernière va mettre en majuscule la première lettre de chaque mot dans la chaîne.

Il est important de noter que ces fonctions ne modifient pas la chaîne d'origine mais en retourne une nouvelle. Si vous souhaitez modifier directement la chaîne, vous pouvez utiliser la fonction `strings.Title()` en combinant les fonctions `strings.Split()` et `strings.Join()`.

## Plongée en Profondeur

La raison pour laquelle les fonctions de capitalisation en Go ne modifient pas directement la chaîne est due à l'immuabilité des chaînes de caractères en Go. Cela signifie qu'une fois qu'une chaîne est créée, elle ne peut pas être modifiée. Toute modification sur une chaîne existante va en créer une nouvelle.

De plus, les fonctions de capitalisation en Go sont sensibles à la langue. Par exemple, la fonction `strings.Title()` va capitaliser correctement les accents et les caractères spéciaux dans les chaînes en français. Cela est dû à l'utilisation de la bibliothèque `unicode` dans le package `strings`.

## Voir Aussi

- https://golang.org/pkg/strings/#ToTitle
- https://golang.org/pkg/strings/#Title
- https://golang.org/pkg/unicode/