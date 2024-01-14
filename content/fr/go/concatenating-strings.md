---
title:    "Go: Concaténation de chaînes"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Pourquoi
Les strings, ou chaînes de caractères, sont un élément important de la programmation. Elles permettent de stocker du texte dans un programme. Dans certains cas, il peut être nécessaire de concaténer, c'est-à-dire de fusionner, plusieurs strings pour obtenir une seule string complète. Dans cet article, nous allons expliquer pourquoi il pourrait être utile de concaténer des strings et comment le faire en utilisant le langage de programmation Go.

## Comment faire
Pour concaténer des strings en Go, il existe plusieurs méthodes. La plus simple consiste à utiliser l'opérateur "+" pour ajouter deux strings ensemble. Par exemple:
```Go
str1 := "Bonjour"
str2 := " le monde!"
concat := str1 + str2

fmt.Println(concat)
// Output: Bonjour le monde!
```

Une autre méthode populaire est d'utiliser la fonction "fmt.Sprintf()" qui permet de formater une string en utilisant des placeholders. Par exemple:
```Go
str1 := "Bonjour"
str2 := "le monde!"
concat := fmt.Sprintf("%s %s", str1, str2)

fmt.Println(concat)
// Output: Bonjour le monde!
```

## Plongée en profondeur
Lorsque l'on concatène des strings, il est important de comprendre que les strings sont immuables en Go. Cela signifie qu'une fois qu'une string est créée, elle ne peut pas être modifiée. Ainsi, lorsque l'on concatène des strings, il est en réalité créé une nouvelle string contenant les deux strings combinées. Cela peut avoir un impact sur les performances si l'on n'y fait pas attention.

De plus, il existe également des packages en Go tels que "bytes" ou "strings" qui offrent des fonctions spécifiques pour manipuler et concaténer des strings de manière plus efficace.

## Voir aussi
- [Documentation officielle sur les strings en Go](https://golang.org/pkg/strings/)
- [Article sur les performances de la concaténation de strings en Go](https://medium.com/@felipedutratine/concatenate-go-string-performance-1d9eb0b4b0e1)
- [Article sur les différents moyens de concaténer des strings en Go](https://blog.learngoprogramming.com/golang-concatenate-strings-efficiently-907c36eeada6)