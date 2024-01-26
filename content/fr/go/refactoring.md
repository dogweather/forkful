---
title:                "Refactoring"
date:                  2024-01-26T01:18:23.149834-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/refactoring.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Le refactoring est le processus de restructuration du code informatique existant sans en changer le comportement externe. Les programmeurs le font pour améliorer les attributs non fonctionnels du logiciel, comme la lisibilité et la maintenabilité, ce qui peut rendre le code plus facile à comprendre, réduire la complexité et aider à repérer plus facilement les bogues.

## Comment faire :
Plongeons dans un exemple simple de refactoring de code en Go. Nous prendrons un extrait qui calcule la moyenne d'un tableau de nombres et le refactoriserons pour plus de clarté et de réutilisabilité.

Code original :
```Go
package main

import "fmt"

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    var sum float64
    for _, num := range numbers {
        sum += num
    }
    average := sum / float64(len(numbers))
    fmt.Println("Moyenne :", average)
}
```

Code refactorisé :
```Go
package main

import "fmt"

// CalculateAverage prend un tableau de float64 et retourne la moyenne.
func CalculateAverage(numbers []float64) float64 {
    sum := 0.0
    for _, num := range numbers {
        sum += num
    }
    return sum / float64(len(numbers))
}

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    average := CalculateAverage(numbers)
    fmt.Println("Moyenne :", average)
}
```

Dans le code refactorisé, nous avons extrait la logique de calcul de la moyenne dans une fonction séparée nommée `CalculateAverage`. Cela rend la fonction `main` plus concise et la logique de calcul de la moyenne réutilisable et testable.

## Exploration Approfondie
Le refactoring de code n'est pas un concept moderne ; il précède l'utilisation généralisée de l'informatique. La pratique a probablement commencé dans le domaine du génie mécanique ou même avant. En logiciel, il est devenu plus formalisé avec l'avènement de la programmation orientée objet et de la programmation extrême (XP) dans les années 1990, notamment influencé par le livre séminal de Martin Fowler "Refactoring: Improving the Design of Existing Code."

Il existe de nombreuses techniques de refactoring, allant du simple renommage de variables pour plus de clarté à des modèles plus complexes comme l'extraction de méthodes ou de classes. L'essentiel est de faire de petits changements progressifs qui ne modifient pas la fonctionnalité du logiciel mais améliorent la structure interne.

Lors de l'utilisation de Go, le refactoring peut être simple en raison de la simplicité du langage et de sa puissante bibliothèque standard. Cependant, il est toujours important d'avoir un bon ensemble de tests unitaires pour s'assurer que le refactoring n'introduise pas de bogues. Des outils comme `gorename` et `gofmt` aident à automatiser une partie du processus, et les IDE ont souvent un support de refactoring intégré.

Outre le refactoring manuel, il existe quelques outils de refactoring de code automatisés disponibles pour Go, tels que les outils de refactoring de GoLand et Go Refactor. Bien qu'ils puissent accélérer le processus, ils ne remplacent pas la compréhension du code et la réalisation de changements considérés.

## Voir Aussi
 - [Refactoring en Go : Simple est Beau](https://go.dev/blog/slices) (lien en anglais)
 - [Go Efficace : Refactoring avec Interfaces](https://go.dev/doc/effective_go#interfaces) (lien en anglais)
 - [Page de Refactoring de Martin Fowler](https://refactoring.com/) (lien en anglais)
 - [Outils de Refactoring GoLand](https://www.jetbrains.com/go/features/refactorings/) (lien en anglais)