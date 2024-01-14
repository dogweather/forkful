---
title:                "Elm: Générer des nombres aléatoires"
programming_language: "Elm"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

La génération de nombres aléatoires est un concept essentiel dans la programmation. Elle est souvent utilisée pour créer des simulations, des jeux ou pour tester des algorithmes. Avec Elm, il est possible de générer des nombres aléatoires de manière simple et efficace.

## Comment faire

Pour générer un nombre aléatoire dans Elm, il faut d'abord importer le module `Random` :

```Elm
import Random
```

Ensuite, il est possible d'utiliser la fonction `generate` pour créer un générateur de nombres aléatoires :

```Elm
generate : (a -> msg) -> Generator a -> Cmd msg
```

Cette fonction prend en paramètre une fonction qui va traiter le nombre aléatoire généré et un générateur qui va déterminer le type et la plage des nombres à générer. Par exemple, pour générer un entier entre 1 et 10, on peut utiliser le générateur `Generator.int 1 10` :

```Elm
generateRandomNumber : Int -> Int -> Cmd msg 
generateRandomNumber min max = 
    Random.generate EnteredNumber (Random.int min max)
```

Pour récupérer le nombre aléatoire et l'utiliser dans notre fonction `EnteredNumber`, on peut utiliser le système de messages et de souscriptions d'Elm.

## En profondeur

Il est important de comprendre que la fonction `generate` ne génère pas de nombre aléatoire directement, mais elle crée un effet qui va être géré par le système d'Elm. En utilisant `Cmd.map`, il est possible de manipuler le nombre aléatoire avant qu'il soit transmis à la fonction de traitement.

De plus, il est recommandé d'utiliser des générateurs globaux plutôt que des générateurs locaux pour éviter la répétition de code.

## Voir aussi

- La documentation officielle sur la génération de nombres aléatoires en Elm : [https://package.elm-lang.org/packages/elm/random/latest/Random](https://package.elm-lang.org/packages/elm/random/latest/Random)
- Un tutoriel sur la génération de nombres aléatoires en Elm : [https://www.codementor.io/@joshuaaroke/how-to-generate-random-numbers-in-elm-3jfgbmy5u](https://www.codementor.io/@joshuaaroke/how-to-generate-random-numbers-in-elm-3jfgbmy5u)
- Un article sur l'utilisation de générateurs globaux en Elm : [https://thoughtbot.com/blog/randomizing-elm](https://thoughtbot.com/blog/randomizing-elm)