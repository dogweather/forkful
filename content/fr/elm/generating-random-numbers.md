---
title:                "Générer des nombres aléatoires"
html_title:           "Elm: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Générer des nombres aléatoires est une action courante dans la programmation qui consiste à produire des nombres au hasard pour une utilisation ultérieure. Les programmeurs utilisent cette technique pour diverses raisons, notamment pour créer des jeux, des simulations et des tests.

## Comment faire:
Voici un exemple de code en Elm pour générer un nombre aléatoire compris entre 1 et 10 et l'afficher dans la console:
```
Elm
import Random

main =    
    Random.int 1 10
        |> Random.generate (\num -> 
            (Debug.log "Nombre aléatoire:" num))
```
Output:
```
Nombre aléatoire: 6
```

## Un peu plus en détail:
La génération de nombres aléatoires est une pratique largement utilisée en informatique depuis de nombreuses années. Avant l'avènement des ordinateurs, des méthodes physiques telles que le lancer de dés ou le choix de cartes étaient utilisées pour générer des nombres aléatoires.

Il existe également d'autres méthodes pour générer des nombres aléatoires en Elm, telles que la génération basée sur une graine (seed) ou en utilisant des algorithmes spécifiques.

## À voir aussi:
- [La documentation officielle d'Elm sur la génération de nombres aléatoires](https://guide.elm-lang.org/effects/random.html)
- [Un tutoriel complet sur la génération de nombres aléatoires en Elm](https://medium.com/@alexmiller/using-random-numbers-in-elm-programs-58a1d4996683)
- [Une discussion sur les différentes méthodes de génération de nombres aléatoires en informatique](https://www.geeksforgeeks.org/pseudo-random-vs-true-random-numbers/)