---
title:    "Go: Recherche et remplacement de texte"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes lors de la programmation en Go. En utilisant les bonnes méthodes, vous pouvez économiser du temps et automatiser une partie de votre travail.

## Comment faire

Voici un exemple de code en Go qui montre comment rechercher et remplacer du texte dans une chaîne de caractères :

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Bonjour tout le monde"

    // Recherche du texte "tout" et remplacement par "tous"
    newStr := strings.Replace(str, "tout", "tous", 1)

    fmt.Println(newStr) // Affiche "Bonjour tous le monde"
}
```

Vous pouvez également rechercher et remplacer du texte dans un fichier en utilisant les fonctions du package `strings` ainsi que les fonctions du package `io` pour lire et écrire dans un fichier.

## Plongée en profondeur

La méthode `Replace` du package `strings` utilise des expressions régulières pour effectuer la recherche et le remplacement de texte. Vous pouvez également utiliser la méthode `Regexp` pour une recherche plus avancée.

Il est également important de noter que la méthode `Replace` retourne une nouvelle chaîne de caractères avec les modifications effectuées, et ne modifie pas la chaîne originale.

## Voir aussi

- Documentation officielle de Go sur les fonctions de remplacement de texte : [https://golang.org/pkg/strings/#Replace](https://golang.org/pkg/strings/#Replace)
- Tutorial sur les expressions régulières en Go : [https://gobyexample.com/regular-expressions](https://gobyexample.com/regular-expressions)
- Plus d'informations sur les méthodes du package `strings` en Go : [https://golang.org/pkg/strings/](https://golang.org/pkg/strings/)