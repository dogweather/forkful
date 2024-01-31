---
title:                "Organisation du code en fonctions"
date:                  2024-01-26T01:10:54.302456-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisation du code en fonctions"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Organiser le code en fonctions consiste à décomposer votre code en éléments réutilisables. Cela rend votre code plus propre, plus facile à lire et plus simple à déboguer.

## Comment faire :
Voici un extrait de code Go qui montre un bloc de code, suivi d'une version refactorisée utilisant des fonctions :

```go
package main

import "fmt"

func main() {
    // Avant : Code intégré
    fmt.Println("Calcul de la somme...")
    total := 0
    for i := 1; i <= 10; i++ {
        total += i
    }
    fmt.Println("La somme totale est :", total)

    // Après : Utilisation d'une fonction
    fmt.Println("Calcul de la somme en utilisant une fonction...")
    somme := getSum(1, 10)
    fmt.Println("La somme totale est :", somme)
}

// Fonction pour calculer la somme dans une plage
func getSum(start, end int) int {
    total := 0
    for i := start; i <= end; i++ {
        total += i
    }
    return total
}
```

Le résultat pour le code intégré et le code basé sur les fonctions sera le même :

```
Calcul de la somme...
La somme totale est : 55
Calcul de la somme en utilisant une fonction...
La somme totale est : 55
```

## Approfondissement
Avant l'émergence du concept de fonctions, la programmation était en grande partie procédurale, avec du code s'exécutant de haut en bas. À mesure que les programmes grandissaient, cette approche provoquait de l'inefficacité et la répétition du code.

Les langages de programmation ont introduit les fonctions comme un mécanisme d'abstraction. En Go, les fonctions encapsulent des blocs de code avec une tâche spécifique, encourageant le principe DRY (Don't Repeat Yourself, Ne vous répétez pas). Elles acceptent des paramètres et peuvent renvoyer des résultats.

Astuces utiles :
- Nommez les fonctions clairement ; un bon nom explique ce que fait une fonction.
- Gardez-les courtes ; si une fonction fait trop de choses, décomposez-la.
- Les fonctions peuvent renvoyer plusieurs valeurs, tirez parti de cela pour la gestion des erreurs.
- Les fonctions d'ordre supérieur (fonctions qui prennent ou renvoient d'autres fonctions) sont des outils puissants en Go.

Les alternatives aux fonctions incluent le code en ligne (désordonné pour les tâches complexes) et les méthodes d'objet (partie du paradigme orienté objet disponible en Go grâce aux structures).

## Voir aussi
- [Go by Example: Functions](https://gobyexample.com/functions)
- [Effective Go: Function](https://golang.org/doc/effective_go#functions)
