---
title:                "Mettre en majuscules une chaîne de caractères"
html_title:           "Go: Mettre en majuscules une chaîne de caractères"
simple_title:         "Mettre en majuscules une chaîne de caractères"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La mise en majuscule d'une chaîne fait référence à la conversion de toutes les lettres d'une chaîne en majuscules. Les programmeurs le font souvent pour normaliser les entrées de l'utilisateur ou pour des raisons d'esthétique dans l'interface utilisateur.

## Comment faire:
Voici un exemple simple de la façon dont vous pouvez mettre en majuscule une chaîne en Go:

```Go 
package main

import (
	"fmt"
	"strings"
)

func main() {
	myString := "bonjour tout le monde"
	fmt.Println(strings.ToUpper(myString))
}
```

Lors de l'exécution de ce code, la sortie sera:

```Go
"BONJOUR TOUT LE MONDE"
```

## Plongée en profondeur

Historiquement, la mise en majuscule d'une chaîne était plus courante dans des langages comme le COBOL où toutes les instructions étaient en majuscules. Aujourd'hui, c'est une pratique courante dans la manipulation des chaînes pour des raisons diverses telles que l'harmonisation et la mise en forme des données. 

En termes d'alternatives, vous pouvez également utiliser `strings.Title()` pour mettre en majuscule la première lettre de chaque mot dans une chaîne. Cependant, pour mettre en majuscule toute la chaîne, `strings.ToUpper()` est la meilleure option en Go. 

En Go, la fonction `strings.ToUpper()` fonctionne en parcourant chaque point de code Unicode dans la chaîne et en le remplaçant par son équivalent en majuscules, si un tel équivalent existe.

## Voir aussi

1. Documentation sur la bibliothèque de chaînes Go : https://pkg.go.dev/strings
2. Plus d'informations sur la mise en majuscule des chaînes : https://golangbyexample.com/uppercase-lowercase-string-golang/