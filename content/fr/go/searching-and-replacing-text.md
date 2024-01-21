---
title:                "Recherche et remplacement de texte"
date:                  2024-01-20T17:57:39.743779-07:00
model:                 gpt-4-1106-preview
simple_title:         "Recherche et remplacement de texte"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Chercher et remplacer du texte est essentiel pour manipuler des données. Les programmeurs le font pour corriger, mettre à jour ou analyser des informations rapidement.

## How to:
```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "Bonjour, le monde! La programmation en Go est amusante."
	replacedText := strings.Replace(text, "amusante", "super", 1)
	fmt.Println(replacedText)
}
```
Sortie :
```
Bonjour, le monde! La programmation en Go est super.
```

## Deep Dive
Le remplacement de texte existe depuis que les gens manipulent du texte sur des ordinateurs – ça date. En Go, la bibliothèque standard `strings` offre des fonctions efficaces pour cela. En dehors de `strings.Replace`, qui change toutes les occurrences, il y a `strings.ReplaceAll` pour une transformation globale. Sous le capot, ces fonctions parcourent le texte, trouvent les correspondances et les remplacent, souvent optimisées pour éviter trop de copies de chaînes de caractères.

## See Also
- Documentation `strings` package: https://pkg.go.dev/strings
- Blog Go about strings manipulation: https://blog.golang.org/strings