---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:48.728472-07:00
description: "Comment faire : Cr\xE9er et initialiser une map en Go peut se faire\
  \ de diff\xE9rentes mani\xE8res. Voici un exemple de base pour commencer ."
lastmod: '2024-03-13T22:44:57.125132-06:00'
model: gpt-4-0125-preview
summary: "Cr\xE9er et initialiser une map en Go peut se faire de diff\xE9rentes mani\xE8\
  res."
title: Utilisation des tableaux associatifs
weight: 15
---

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
