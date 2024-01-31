---
title:                "Interpolation de chaînes de caractères"
date:                  2024-01-20T17:51:02.336118-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolation de chaînes de caractères"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
L'interpolation de chaînes permet d'insérer des variables dans des chaînes de texte. On s'en sert pour construire des messages dynamiques sans jongler avec la concaténation compliquée.

## How to:
En Go, on interpole avec `fmt.Sprintf`. Simple et efficace.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Variables
	name := "Marie"
	age := 29
	
	// Interpolation avec fmt.Sprintf
	message := fmt.Sprintf("Bonjour, %s ! Tu as %d ans.", name, age)
	fmt.Println(message)

	// Utiliser avec des types complexes
	currentTime := time.Now()
	dateMessage := fmt.Sprintf("Nous sommes le %s.", currentTime.Format("02-01-2006"))
	fmt.Println(dateMessage)
}
```
Sortie attendue:
```
Bonjour, Marie ! Tu as 29 ans.
Nous sommes le 24-03-2023.
```

## Deep Dive
Historiquement, Go utilise toujours `fmt.Sprintf` pour l'interpolation depuis sa sortie. Alors que certains langages utilisent `+` ou diverses méthodes, Go choisit la simplicité.

Autres méthodes :
- Concaténation : `"Salut " + name + " !"`
- Builder : `strings.Builder` pour les grandes concaténations

Implémentation :
- Utilise `verb` (ex : `%s` pour `string`, `%d` pour `int`) pour définir comment interpoler.
- L'interpolation est type-safe ; pas de surprises de casting implicite.

## See Also

- La documentation officielle de `fmt`: [Package fmt](https://pkg.go.dev/fmt)
- Des exemples sur 'Go by Example': [String Formatting](https://gobyexample.com/string-formatting)
- Un tutoriel sur 'Go Blog': [Using Go Modules](https://blog.golang.org/using-go-modules) (module handling can indirectly affect string operations)
