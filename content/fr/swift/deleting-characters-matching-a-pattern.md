---
title:                "Supprimer les caractères correspondant à un modèle"
html_title:           "Ruby: Supprimer les caractères correspondant à un modèle"
simple_title:         "Supprimer les caractères correspondant à un modèle"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Supprimer les caractères correspondant à un motif (pattern matching) est un moyen d'éliminer spécifiquement cet ensemble de caractères d'une chaîne dans la programmation Swift. Les programmeurs le font pour nettoyer les données, préserver l'exactitude des calculs ou faciliter le traitement et l'analyse des données.

## Comment faire:

Pour supprimer les caractères correspondant à un motif, nous utilisons la fonction `filter`. Par exemple:
```Swift
let str = "Salut, mon ami!"
let filtreStr = str.filter { !" ,!".contains($0) }
print(filtreStr)
```
Cela va supprimer les espaces, les virgules et les points d'exclamation et produira le résultat suivant:
```Swift
Salutmonami
```
## Plongeons Profondément

Historiquement, il a toujours été crucial pour les programmeurs de manipuler et de contrôler les chaînes. Swift fournit une manipulation de chaînes puissante et flexible. 

Une alternative à la fonction `filter` pourrait être d'utiliser une boucle `for-in` pour itérer sur la chaîne et construire une nouvelle chaîne sans les caractères indésirables. Cependant, cela demande plus de lignes de code.

En ce qui concerne les détails de l'implémentation, `filter` en Swift retourne un nouvel array contenant des éléments pour lesquels un fournisseur de conditions booléennes indique true. Dans notre cas, elle retourne une nouvelle chaîne sans les caractères correspondant à notre motif.

## Voir Aussi:

Pour plus d'information, visitez ces liens:

1. Le Guide de la Programmation Swift par Apple (Disponible gratuitement sur l'Apple Store): [Guide Swift](https://developer.apple.com/swift/)