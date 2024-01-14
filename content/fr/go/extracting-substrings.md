---
title:                "Go: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi
Dans la programmation Go, il est parfois nécessaire d'extraire une partie spécifique d'une chaîne de caractères, également appelée "substring". Cela peut être utile pour diverses raisons, telles que la manipulation de données ou la recherche de motifs dans une chaîne. Dans cet article, nous explorerons comment extraire des substrings en utilisant Go.

## Comment faire
Pour extraire une substring en utilisant Go, nous pouvons utiliser la fonction `Substring()` de la bibliothèque standard `Strings`. Voici un exemple de code montrant comment extraire une portion de chaîne à partir de la position 3 jusqu'à la fin :

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Hello world"
    sub := strings.SubString(str, 3, len(str))
    fmt.Println(sub)
}

```
La sortie de ce code sera "lo world". Nous spécifions la chaîne d'origine en premier paramètre, suivi de l'index de départ et de l'index de fin pour la substring que nous voulons extraire. Nous pouvons également utiliser des nombres négatifs pour compter à partir de la fin de la chaîne.

Voici un autre exemple montrant comment extraire une partie de la chaîne à partir de l'index de début jusqu'à une longueur spécifique :

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Bonjour le monde"
    sub := strings.SubString(str, 8, 3)
    fmt.Println(sub)
}

```
La sortie sera "le". Nous spécifions également la chaîne d'origine en premier paramètre, suivi de l'index de départ et de la longueur de la substring que nous voulons extraire.

## Deep Dive
Il est important de noter que les indices utilisés pour extraire des substrings en Go sont basés sur les octets et non sur les caractères. Cela signifie que si notre chaîne contient des caractères codés sur plusieurs octets, les index peuvent ne pas correspondre à la position visible du caractère dans la chaîne.

En utilisant la fonction `Substring()` de la bibliothèque standard, nous pouvons également extraire des sous-chaînes à partir d'une chaîne en utilisant des expressions régulières. Cela peut être utile pour extraire des motifs spécifiques dans une chaîne de caractères.

## Voir aussi
- [Documentation officielle de la fonction `Substring()`](https://golang.org/pkg/strings/#Substring)
- [Un tutoriel sur l'utilisation des expressions régulières en Go](https://blog.alexellis.io/golang-writing-unit-tests/)
- [Un guide sur la manipulation de chaînes en Go](https://tutorialedge.net/golang/working-with-strings-in-go/)