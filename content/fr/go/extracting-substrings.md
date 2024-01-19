---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi?

Extraire des sous-chaînes signifie prendre une chaîne de caractères et en retirer une partie. Nous faisons cela pour manipuler et transformer des informations spécifiques à partir de chaînes de caractères plus longues.

## Comment faire:

La standard « strings » bibliothèque dans Go facilite l'extraction des sous-chaînes. Utilisez l'opérateur de découpage pour faire cela. Voici un petit example:

```Go
package main
import "fmt"

func main() {
    str := "Bonjour, programmeurs!"
    fmt.Println(str[9:22])  // sortie: "programmeurs"
}
```

Dans cet exemple, nous avons une chaîne "Bonjour, programmeurs!" et nous en extrayons la sous-chaîne "programmeurs!". L'indexation commence à 0, donc `str[9:22]` nous donne la sous-chaîne à partir du 9ème au 22ème caractère.

## Plongée en profondeur:

Historiquement, les chaînes de caractères en Go sont immuables, c'est-à-dire qu'une fois qu'elles sont créées, elles ne peuvent pas être modifiées. C'est pour cette raison que nous extrayons des sous-chaînes - pour manipuler les données sans changer la chaîne originale.

Il existe plusieurs alternatives à l'extraction de sous-chaînes en utilisant l'opérateur de découpage. L'une d'entre elles est l'utilisation de la fonction `strings.Split` qui divise une chaîne en plusieurs sous-chaînes en fonction d'un séparateur donné.

En Go, l'extraction de sous-chaînes est une opération en temps constant, ce qui signifie que la complexité de l'extraction de sous-chaînes est O(1). Cela est possible en raison de l'implémentation interne des chaînes en Go, où une sous-chaîne partage la mémoire avec la chaîne originale.

## Voir aussi:

1. Documentation officielle de Go sur les chaînes de caractères: https://golang.org/pkg/strings
2. Plus d'informations sur l'utilisation des sous-chaînes en Go: https://gobyexample.com/string-functions
3. Utiliser l'opérateur de découpage avec les chaînes en Go: https://yourbasic.org/golang/slice-string/