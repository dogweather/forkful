---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:31.779228-07:00
description: "Comment faire : Dans Go, vous d\xE9finissez une fonction en utilisant\
  \ le mot-cl\xE9 `func`, suivi du nom de la fonction, des param\xE8tres (s'il y en\
  \ a) et du type\u2026"
lastmod: '2024-03-13T22:44:57.139957-06:00'
model: gpt-4-0125-preview
summary: "Dans Go, vous d\xE9finissez une fonction en utilisant le mot-cl\xE9 `func`,\
  \ suivi du nom de la fonction, des param\xE8tres (s'il y en a) et du type de retour."
title: Organiser le code en fonctions
weight: 18
---

## Comment faire :
Dans Go, vous définissez une fonction en utilisant le mot-clé `func`, suivi du nom de la fonction, des paramètres (s'il y en a) et du type de retour. Illustrons avec un exemple simple :

```go
package main

import "fmt"

// définir une fonction pour calculer la somme de deux nombres
func addNumbers(a int, b int) int {
    return a + b
}

func main() {
    sum := addNumbers(5, 7)
    fmt.Println("La somme est :", sum)
    // Sortie : La somme est : 12
}
```

Les fonctions peuvent également retourner plusieurs valeurs, ce qui est une caractéristique unique par rapport à de nombreux autres langages. Voici comment vous pouvez en profiter :

```go
// définir une fonction pour échanger deux nombres
func swap(a, b int) (int, int) {
    return b, a
}

func main() {
    x, y := swap(10, 20)
    fmt.Println("x, y après échange :", x, y)
    // Sortie : x, y après échange : 20 10
}
```

Vous pouvez également définir des fonctions avec un nombre variable d'arguments en utilisant les points de suspension `...` avant le type de paramètre. Ceci est utile pour créer des fonctions flexibles :

```go
// définir une fonction pour calculer la somme d'un nombre inconnu d'entiers
func sum(numbers ...int) int {
    total := 0
    for _, number := range numbers {
        total += number
    }
    return total
}

func main() {
    total := sum(1, 2, 3, 4, 5)
    fmt.Println("Le total est :", total)
    // Sortie : Le total est : 15
}
```

## Approfondissement
Le concept d'organiser le code en fonctions n'est pas propre à Go - c'est un principe fondamental de programmation. Cependant, Go introduit certaines conventions et capacités qui distinguent sa gestion de fonction. Par exemple, la capacité de retourner plusieurs valeurs des fonctions est relativement unique et peut conduire à un code plus propre et plus compréhensible, particulièrement lors de la gestion d'opérations qui pourraient traditionnellement nécessiter l'utilisation de pointeurs ou de gestion des exceptions.

De plus, le soutien de Go pour les fonctions de première classe – des fonctions qui peuvent être passées comme arguments à d'autres fonctions, renvoyées comme valeurs de fonctions et assignées à des variables – amplifie le soutien du langage pour les motifs de programmation fonctionnelle. Cette caractéristique est particulièrement utile pour créer des fonctions d'ordre supérieur qui manipulent ou combinent d'autres fonctions.

Cependant, il est essentiel de tenir compte de la "loi des rendements décroissants" lorsque l'on organise le code en fonctions. Une sur-modularisation peut conduire à une abstraction excessive, rendant le code plus difficile à comprendre et à maintenir. De plus, bien que l'approche simpliste de Go pour la gestion des erreurs (retourner des erreurs comme valeurs de retour normales) encourage une propagation d'erreur propre à travers plusieurs couches d'appels de fonction, cela peut conduire à un code de gestion des erreurs répétitif. Des alternatives comme les cadres de gestion des erreurs ou l'adoption de l'approche "try-catch" d'autres langages (bien que non nativement pris en charge) via des implémentations de paquets peuvent parfois offrir des solutions plus élégantes en fonction du cas d'utilisation.

La décision d'utiliser les fonctions et la modularisation dans Go de manière extensive devrait équilibrer le besoin d'abstraction, de maintenabilité, de performance et de gestion des erreurs lisible, en tirant le meilleur parti des fonctionnalités simples, mais puissantes de Go.
