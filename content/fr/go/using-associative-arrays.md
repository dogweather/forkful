---
title:                "Utilisation des tableaux associatifs"
date:                  2024-01-30T19:11:21.885033-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des tableaux associatifs"

category:             "Go"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les tableaux associatifs, connus sous le nom de maps en Go, vous permettent de stocker et d'accéder à des données avec des paires clé-valeur. Ils sont essentiels pour gérer des collections où vous pouvez rechercher rapidement des valeurs par une clé unique, simplifiant ainsi la manipulation et la récupération des données dans vos programmes.

## Comment faire :

En Go, les maps sont simples à utiliser. Voici un guide simple pour commencer :

1. **Déclaration et Initialisation des Maps**

```Go
package main

import "fmt"

func main() {
    // Initialise une map vide avec des clés de type string et des valeurs de type int
    var scores map[string]int
    fmt.Println(scores) // Affiche : map[]

    // Déclaration et initialisation d'une map non vide
    colors := map[string]string{
        "red": "#ff0000",
        "green": "#00ff00",
    }
    fmt.Println(colors) // Affiche : map[green:#00ff00 red:#ff0000]
}
```

2. **Ajout et Accès aux Éléments**

```Go
func main() {
    fruits := make(map[string]int)
    fruits["apples"] = 5
    fruits["bananas"] = 10

    fmt.Println(fruits["apples"]) // Affiche : 5
}
```

3. **Itération sur les Maps**

```Go
func main() {
    pets := map[string]string{"dog": "aboiement", "cat": "miaou"}

    for key, value := range pets {
        fmt.Printf("%s fait %s\n", key, value)
    }
    // L'ordre de sortie peut varier, car les maps ne garantissent pas l'ordre.
}
```

4. **Suppression d'Éléments**

```Go
func main() {
    meals := map[string]int{"breakfast": 300, "lunch": 600}
    fmt.Println(meals) // Avant la suppression

    delete(meals, "lunch")
    fmt.Println(meals) // Après la suppression
}
```

## Plongée en Profondeur

Introduites dans Go 1, les maps fournissent un moyen intégré de gérer efficacement les tableaux associatifs. Contrairement aux slices, qui sont des collections ordonnées, les maps sont désordonnées. Cela signifie que l'ordre d'itération sur les éléments de la map n'est pas garanti d'être le même à travers les exécutions, un compromis pour sa capacité à gérer dynamiquement les paires clé-valeur avec une flexibilité significative.

Sous le capot, Go implémente les maps comme des tables de hachage, assurant que la complexité moyenne des opérations d'accès, d'insertion et de suppression est O(1), dans la plupart des circonstances. Cependant, il convient de noter que cette efficacité peut varier en fonction de facteurs tels que les collisions de hachage.

Pour les cas d'utilisation nécessitant un parcours clé ordonné, vous pourriez envisager de combiner les maps avec des slices ou d'explorer des packages tiers qui offrent des structures de données supplémentaires comme des maps ordonnées ou des arbres. Malgré leurs limitations, les maps de Go sont un outil puissant et essentiel pour de nombreux scénarios de programmation.
