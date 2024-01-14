---
title:                "Go: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est courant en programmation de devoir manipuler des chaînes de caractères, et parfois, il peut être nécessaire de les convertir en lettres minuscules pour des raisons telles que la comparaison de chaînes. Dans cet article, nous allons explorer comment effectuer cette opération en utilisant le langage de programmation Go.

## Comment faire

Nous pouvons utiliser la fonction `strings.ToLower()` pour convertir une chaîne de caractères en lettres minuscules. Voici un exemple de code Go:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Exemple de CHAÎNE de Caractères"
	lowerCaseStr := strings.ToLower(str)
	fmt.Println(lowerCaseStr)
}

```

La sortie de ce code sera `exemple de chaîne de caractères`. Nous importons d'abord le package `strings` pour avoir accès à la fonction `ToLower()` et ensuite nous déclarons une variable `str` contenant une chaîne de caractères. Nous utilisons ensuite la fonction `ToLower()` sur cette variable et stockons le résultat dans une nouvelle variable `lowerCaseStr` qui sera ensuite imprimée.

## Plongée en profondeur

Lorsqu'une chaîne de caractères est convertie en lettres minuscules, il est important de comprendre comment cette opération est effectuée. En réalité, les chaînes de caractères dans Go sont des tableaux d'octets (bytes) et la fonction `ToLower()` itère à travers chaque octet et modifie ceux qui sont des lettres majuscules en les convertissant en lettres minuscules. Cette fonction prend également en compte les caractères spéciaux et les langues qui peuvent avoir des règles de conversion différentes.

## Voir aussi

- [Documentation officielle de la fonction `ToLower()`](https://golang.org/pkg/strings/#ToLower)
- [Tutoriel sur les chaînes en Go](https://www.tutorialspoint.com/go/go_strings.htm)
- [Exemple de fonction de conversion de lettres majuscules en minuscules en Go](https://yourbasic.org/golang/convert-string-to-lower-upper-title-camel-case/)