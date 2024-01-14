---
title:                "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères est une tâche très courante en programmation. Cela peut être utile lors de la manipulation de données ou de la création d'algorithmes. Dans cet article, nous allons apprendre comment trouver la longueur d'une chaîne en utilisant le langage de programmation Go.

## Comment Faire

Pour trouver la longueur d'une chaîne en Go, nous utilisons la fonction `len()` qui renvoie le nombre de caractères dans la chaîne. Voici un exemple de code pour illustrer cela:

```Go
package main

import "fmt"

func main() {
    text := "Bonjour le monde"
    fmt.Println("La longueur de la chaîne est:", len(text))
}
```

Ce code définit une variable `text` avec la chaîne "Bonjour le monde" et utilise la fonction `len()` pour renvoyer sa longueur. Lorsque nous exécutons ce code, nous obtenons l'output suivant:

```
La longueur de la chaîne est: 16
```

Nous pouvons également utiliser la boucle `for` pour parcourir chaque caractère de la chaîne et incrémenter un compteur pour trouver sa longueur. Voici un exemple de code pour cela:

```Go
package main

import "fmt"

func main() {
    text := "Bonjour le monde"
    var count int
    for range text {
        count++
    }
    fmt.Println("La longueur de la chaîne est:", count)
}
```

Ce code utilise la boucle `for` pour parcourir chaque caractère de `text` et incrémenter le compteur `count`. Lorsque la boucle se termine, `count` contient la longueur de la chaîne. L'output de ce code sera également "La longueur de la chaîne est: 16".

## Plongée Profonde

En Go, les chaînes de caractères sont en fait des tableaux de bytes. La fonction `len()` renvoie donc le nombre de bytes et non le nombre de caractères. Cela signifie que pour les chaînes contenant des caractères multibytes, comme les caractères accentués ou les emojis, la longueur renvoyée par `len()` peut être différente de ce à quoi nous nous attendons. Pour trouver le nombre de caractères réel, nous pouvons utiliser la fonction `utf8.RuneCountInString()`.

## Voir Aussi

- [Documentation officielle de Go sur les chaînes de caractères](https://golang.org/ref/spec#String_types)
- [Tutoriel sur les chaînes de caractères en Go](https://www.golangprograms.com/golang-strings.html)
- [Exemples pratiques de manipulation de chaînes de caractères en Go](https://www.calhoun.io/6-tips-for-using-strings-in-go/)