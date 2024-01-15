---
title:                "Extraction de sous-chaînes"
html_title:           "Go: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec du texte dans vos programmes Go, vous pouvez parfois avoir besoin d'extraire des parties spécifiques de ce texte, appelées sous-chaînes ou substrings. Cela peut être utile pour effectuer des opérations spécifiques sur ces parties, comme la recherche ou le remplacement de mots.

## Comment faire

Pour extraire une sous-chaîne dans Go, vous devez utiliser la fonction `substring` en spécifiant l'index de début et l'index de fin de la sous-chaîne souhaitée, comme ceci:

```Go
str := "Bonjour tout le monde"
fmt.Println(str[8:13]) // Cela affichera "tout"
```

Vous pouvez également utiliser un index négatif pour commencer à compter depuis la fin de la chaîne, comme ceci:

```Go
str := "Salut les amis"
fmt.Println(str[5:len(str)]) // Cela affichera "les amis"
```

Il est également possible de spécifier seulement l'index de début ou de fin de la sous-chaîne, comme ceci:

```Go
str := "Hello everyone"
fmt.Println(str[:5]) // Cela affichera "Hello"
fmt.Println(str[6:]) // Cela affichera "everyone"
```

## Plongée en profondeur

En plus de la fonction `substring`, il existe également la fonction `slice`, qui permet d'extraire plusieurs sous-chaînes à la fois. Cette fonction prend en paramètre un tableau d'indices, ce qui signifie que vous pouvez extraire plusieurs parties du texte en une seule fois. Voici un exemple:

```Go
str := "Je suis un développeur"
indexes := []int{3, 7, 11}
fmt.Println(str[indexes[0]:indexes[1]]) // Cela affichera "suis"
fmt.Println(str[indexes[1]:indexes[2]]) // Cela affichera "un"
```

Il est également important de noter que lorsqu'un indice est spécifié en dehors de la longueur de la chaîne, cela provoquera une erreur. Il est donc important de faire attention aux index utilisés lors de l'extraction de sous-chaînes.

## Voir aussi

- [Documentation officielle de la fonction substring](https://golang.org/ref/spec#Slice_expressions)
- [Guide pour travailler avec des chaînes de caractères en Go](https://yourbasic.org/golang/string-functions-reference-cheat-sheet/)