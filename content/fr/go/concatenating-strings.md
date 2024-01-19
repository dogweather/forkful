---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concaténation des chaînes en Go

## Quoi & Pourquoi?
La concaténation des chaînes est l'action de joindre deux (ou plus) chaînes en une seule. Les programmeurs l'utilisent souvent pour combiner du texte et des variables dans des messages, des logs ou des analyses de données.

## Comment faire:
Voici comment concaténer des chaînes en Go. 

```Go
package main

import "fmt"

func main() {
    s1 := "Bonjour, "
    s2 := "monde!"

    rs := s1 + s2 // Concaténation

    fmt.Println(rs) // "Bonjour, monde!"
}
```
Et voici un cas d'une variable intégrée dans une chaîne :

```Go
package main

import "fmt"

func main() {
    name := "Jean"

    greeting := "Bonjour, " + name + "!"

    fmt.Println(greeting) // "Bonjour, Jean!"
}
```
## Plongée en profondeur
Historiquement, Go a été conçu pour être simple et efficace, même dans la gestion des chaînes. Par rapport à d'autres langages comme Java et Python, Go est plus rapide mais moins flexible en ce qui concerne la concaténation des chaînes. 

Une alternative à l'opérateur `+` pour la concaténation est la fonction `fmt.Sprintf()`, qui permet d'intégrer une variable dans une chaîne de manière plus contrôlée.

```Go
package main

import "fmt"

func main() {
    name := "Jean"

    greeting := fmt.Sprintf("Bonjour, %s!", name)

    fmt.Println(greeting) // "Bonjour, Jean!"
}
```

Cependant, selon le cas d'usage, l'une ou l'autre méthode pourrait être plus appropriée. Par exemple, la concaténation avec `+` est simple et rapide, mais `fmt.Sprintf()` offre plus de contrôle et de flexibilité.

## Voir aussi
- [Documentation officielle de Go](https://golang.org/)
- [Go by Example: String Formatting](https://gobyexample.com/string-formatting)
- [Post de blog: "Effective Go" sur golang.org](https://golang.org/doc/effective_go)