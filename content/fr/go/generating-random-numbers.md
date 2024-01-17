---
title:                "La génération de nombres aléatoires"
html_title:           "Go: La génération de nombres aléatoires"
simple_title:         "La génération de nombres aléatoires"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Générer des nombres aléatoires est une tâche souvent utilisée par les programmeurs pour créer des éléments aléatoires dans leurs programmes. Cela peut être utile dans les jeux, les simulations, les tests et bien d'autres applications.

## Comment faire:

Voici un exemple de code en Go pour générer un nombre aléatoire entre 1 et 10:

```Go
import "math/rand"
import "time"

func main() {
    // nous devons définir une "graine" pour le générateur aléatoire
    rand.Seed(time.Now().UnixNano())

    // maintenant, nous pouvons utiliser la fonction rand.Intn pour obtenir un nombre aléatoire
    // entre 1 et 10
    num := rand.Intn(10) + 1
    fmt.Println(num) // output: un nombre aléatoire entre 1 et 10
}
```

## Plongée en profondeur:

Générer des nombres aléatoires est une pratique courante en informatique depuis de nombreuses années. Avant l'avènement des ordinateurs, des méthodes telles que le tirage au sort de nombres dans un chapeau étaient utilisées pour obtenir des résultats aléatoires.

En dehors de la bibliothèque intégrée de Go pour générer des nombres aléatoires, il existe d'autres alternatives telles que la bibliothèque "crypto/rand" qui utilise des méthodes de cryptographie pour garantir une meilleure aléatorité.

## A voir également:

Si vous souhaitez en savoir plus sur la génération de nombres aléatoires en Go, voici quelques ressources utiles:

- [Documentation officielle de Go pour les fonctions de génération de nombres aléatoires](https://golang.org/pkg/math/rand/)
- [Utilisation de la bibliothèque "crypto/rand" pour une meilleure aléatorité en Go](https://golang.org/pkg/crypto/rand/)