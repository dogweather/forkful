---
title:                "Supprimer les caractères correspondant à un modèle"
html_title:           "Go: Supprimer les caractères correspondant à un modèle"
simple_title:         "Supprimer les caractères correspondant à un modèle"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Supprimer des caractères correspondant à un modèle est une opération courante en programmation qui consiste à supprimer tous les caractères dans une chaîne de caractères qui matchent un certain modèle, tel qu'une lettre, un chiffre ou un symbole spécifique. Les programmeurs le font souvent pour nettoyer et normaliser des données ou pour analyser des chaînes de caractères complexes.

## Comment faire:

Voici un exemple en Go pour supprimer tous les chiffres d'une chaîne de caractères:

```
package main

import (
	"fmt"
	"regexp"
)

func main() {
	input := "Hello 123 World!"
	output := regexp.MustCompile("\\d+").ReplaceAllString(input, "")
	fmt.Println(output)
}

// Output: Hello World!
```

Voici un autre exemple pour supprimer tous les symboles d'une chaîne de caractères:

```
package main

import (
	"fmt"
	"regexp"
)

func main() {
	input := "Hello!$# World!"
	output := regexp.MustCompile("[^a-zA-Z0-9 ]+").ReplaceAllString(input, "")
	fmt.Println(output)
}

// Output: Hello World
```

## Plongée en profondeur:

Supprimer des caractères correspondant à un modèle existe depuis le début de la programmation et est souvent utilisé pour nettoyer des données. Une alternative courante à l'utilisation d'expressions régulières est d'utiliser des boucles et des conditions pour parcourir chaque caractère de la chaîne et supprimer ceux qui correspondent au modèle. En termes d'implémentation, la fonction ReplaceAllString de la bibliothèque regexp de Go utilise l'algorithme de substitution de Boyer-Moore pour améliorer les performances.

## Voir aussi:

Pour en savoir plus sur les expressions régulières en Go, vous pouvez consulter la documentation officielle: https://golang.org/pkg/regexp/
Vous pouvez également lire cet article sur les bonnes pratiques pour utiliser les expressions régulières en Go: https://flaviocopes.com/golang-regexp/