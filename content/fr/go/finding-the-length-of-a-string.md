---
title:                "Trouver la longueur d'une chaîne de caractères"
date:                  2024-01-20T17:47:24.856201-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trouver la longueur d'une chaîne de caractères"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Trouver la longueur d'une chaîne de caractères, c'est déterminer combien de caractères elle contient. C'est essentiel pour manipuler des textes – par exemple, pour valider des saisies ou découper des chaînes.

## How to:
```Go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	str := "Salut 👋"
	fmt.Println("Bytes count:", len(str))
	fmt.Println("Runes count:", utf8.RuneCountInString(str))
}
```
Résultat:
```
Bytes count: 11
Runes count: 6
```

## Deep Dive
Autrefois en Go, `len(str)` aurait suffi pour obtenir la longueur. Mais avec l'internationalisation et l'UTF-8, ça se complique : chaque caractère ('rune' en Go) peut prendre plus d'un octet. D'où `utf8.RuneCountInString(str)` pour compter correctement les caractères. Certains langages ont des fonctions intégrées qui gèrent ça internement, mais en Go, c'est à nous de choisir la bonne approche.

## See Also
- Documentation Go sur les chaînes : https://golang.org/pkg/strings/
- Unicode et UTF-8 en Go : https://blog.golang.org/strings
- Package utf8 : https://golang.org/pkg/unicode/utf8/
