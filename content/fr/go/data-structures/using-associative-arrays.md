---
title:                "Utilisation des tableaux associatifs"
aliases:
- /fr/go/using-associative-arrays/
date:                  2024-02-03T18:10:48.728472-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des tableaux associatifs"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/using-associative-arrays.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les tableaux associatifs, connus sous le nom de maps en Go, vous permettent de stocker des paires clé-valeur où chaque clé unique est associée à une valeur. Les programmeurs utilisent les maps pour récupérer, modifier des données efficacement, et pour maintenir une collection d'éléments qui peuvent être rapidement accédés en utilisant des clés uniques.

## Comment faire :

Créer et initialiser une map en Go peut se faire de différentes manières. Voici un exemple de base pour commencer :

```go
package main

import "fmt"

func main() {
    // Déclarer et initialiser une map
    colors := map[string]string{
        "rouge": "#FF0000",
        "vert":  "#00FF00",
        "bleu":  "#0000FF",
    }

    fmt.Println(colors)
    // Sortie : map[bleu:#0000FF vert:#00FF00 rouge:#FF0000]
}
```

Pour ajouter ou mettre à jour des éléments, vous assignez une valeur à une clé comme ceci :

```go
colors["blanc"] = "#FFFFFF"
fmt.Println(colors)
// Sortie : map[bleu:#0000FF vert:#00FF00 rouge:#FF0000 blanc:#FFFFFF]
```

Accéder à une valeur par sa clé est simple :

```go
fmt.Println("Le code hexadécimal pour le rouge est :", colors["rouge"])
// Sortie : Le code hexadécimal pour le rouge est : #FF0000
```

Pour supprimer un élément, utilisez la fonction `delete` :

```go
delete(colors, "rouge")
fmt.Println(colors)
// Sortie : map[bleu:#0000FF vert:#00FF00 blanc:#FFFFFF]
```

Itérer sur une map se fait en utilisant une boucle for :

```go
for couleur, hex := range colors {
    fmt.Printf("Clé : %s Valeur : %s\n", couleur, hex)
}
```

Rappelez-vous, les maps en Go ne sont pas ordonnées. L'ordre d'itération n'est pas garanti.

## Plongée Profonde

En Go, les maps sont implémentées comme des tables de hachage. Chaque entrée dans la map consiste en deux éléments : une clé et une valeur. La clé est hachée pour stocker l'entrée, ce qui permet des opérations en temps constant pour un petit ensemble de données et une complexité temporelle moyenne de O(1) avec un hachage adéquat, pouvant se dégrader à O(n) dans le pire des cas avec de nombreuses collisions de hachage.

Une note importante pour les nouveaux programmeurs en Go est que les types de map sont des types de référence. Cela signifie que lorsque vous passez une map à une fonction, tout changement apporté à la map au sein de cette fonction est visible par l'appelant. C'est différent, par exemple, de passer une structure à une fonction, où la structure est copiée à moins qu'elle ne soit passée par un pointeur.

Alors que les maps sont incroyablement polyvalentes et efficaces pour la plupart des cas d'utilisation impliquant des tableaux associatifs, dans les applications où la performance est critique, il peut être bénéfique d'utiliser des structures de données ayant des caractéristiques de performance plus prévisibles, surtout si les distributions de clés peuvent causer des collisions fréquentes.

Une autre alternative à considérer est le `sync.Map`, disponible depuis Go 1.9, conçu pour les cas d'utilisation où les clés sont écrites une seule fois mais lues de nombreuses fois, offrant des améliorations d'efficacité dans ces scénarios. Cependant, pour les applications Go conventionnelles, l'utilisation régulière de map est idiomatique et souvent l'approche recommandée pour sa simplicité et son soutien direct dans le langage.
