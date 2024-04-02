---
date: 2024-01-26 01:11:54.072879-07:00
description: "Regrouper le code en fonctions, c'est d\xE9composer les t\xE2ches en\
  \ morceaux r\xE9utilisables. Cela rend le code propre, moins sujet aux erreurs et\
  \ plus facile \xE0\u2026"
lastmod: '2024-03-13T22:44:58.228552-06:00'
model: gpt-4-1106-preview
summary: "Regrouper le code en fonctions, c'est d\xE9composer les t\xE2ches en morceaux\
  \ r\xE9utilisables. Cela rend le code propre, moins sujet aux erreurs et plus facile\
  \ \xE0\u2026"
title: Organisation du code en fonctions
weight: 18
---

## Quoi & Pourquoi ?
Regrouper le code en fonctions, c'est décomposer les tâches en morceaux réutilisables. Cela rend le code propre, moins sujet aux erreurs et plus facile à déboguer ou à refactoriser.

## Comment faire :
Imaginez une tâche : calculer la moyenne d'un tableau. Sans fonctions, vous mettriez tout dans le main. Avec les fonctions, vous feriez ceci :

```swift
func calculateAverage(of numbers: [Double]) -> Double {
    let sum = numbers.reduce(0, +)
    return numbers.isEmpty ? 0 : sum / Double(numbers.count)
}

// Utilisation
let scores = [92.5, 88.75, 99.0, 70.5]
let averageScore = calculateAverage(of: scores)
print("La moyenne des scores est \(averageScore)")
```

La sortie exemple serait :
```
La moyenne des scores est 87.6875
```

## Approfondissement
Historiquement, à mesure que la programmation se complexifiait, les fonctions sont devenues une pierre angulaire pour gérer la complexité. Les alternatives incluent le codage en ligne et le copier-coller de code (code spaghetti) – maintenant largement considérés comme une mauvaise pratique. Dans Swift, les fonctions sont des citoyens de première classe; elles peuvent être assignées à des variables, passées en tant qu'arguments et retournées par d'autres fonctions, rendant le code plus modulaire et flexible.

En termes de mise en œuvre, concevez vos fonctions pour faire bien une chose. Visez des fonctions avec un but clair et un nom qui le reflète. Faites attention au nombre de paramètres - s'il y en a trop, vous faites probablement trop de choses. Gestion des erreurs ? Envisagez des fonctions avec gestion d'exceptions et gérez les problèmes avec élégance. Souvenez-vous : Swift est tout au sujet de la lisibilité et de la facilité de maintenance.

## Voir également
- [Guide du langage de programmation Swift - Fonctions](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Guide de style Swift de Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)
- [Refactoring : Améliorer la conception du code existant de Martin Fowler](https://martinfowler.com/books/refactoring.html)
