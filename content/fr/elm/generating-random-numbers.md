---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

---

# Génération de nombres aléatoires en Elm

## Qu'est-ce que c'est et pourquoi ?

L'opération de générer des nombres aléatoires est une méthode pour produire des séquences de nombres sans aucun motif apparent. Les programmeurs l’utilisent fréquemment pour simuler des événements aléatoires, tester une fonctionnalité de manière exhaustive, ou encore pour la sécurité et le cryptage.

## Comment faire :

Elm ne génère pas de nombres aléatoires de façon directe comme d'autres langages, à la place, il les génère à travers les `Tasks`. Voici un exemple de comment obtenir un nombre aléatoire entre 1 et 100 en Elm :

```Elm
import Random
import Task

main =
    Random.generate identity (Random.int 1 100)
        |> Task.attempt (always identity)
        |> Html.program
```

Dans cet exemple, la commande `Random.int 1 100` génère une `Task` qui produira un nombre aléatoire entre 1 et 100.

## Plongée en profondeur

Historiquement, la génération de nombres aléatoires était une tâche simple dans la plupart des langages de programmation. Mais comme Elm applique le concept de pureté fonctionnelle de manière rigoureuse, cette tâche est un peu plus complexe qu’à l'accoutumée.

Les alternatives à Elm pour générer des nombres aléatoires comprennent des langages de programmation plus traditionnels comme Python ou Java.

Concernant les détails d'implémentation, Elm utilise une version modifiée de l'algorithme de Mersenne Twister pour générer des nombres aléatoires. Les réponses à ces `Tasks` sont indirectes, ce qui signifie qu'elles ne retournent pas de résultat immédiat, mais une description de ce qu'il faut faire pour obtenir le résultat. Ainsi, les nombres aléatoires doivent être récupérés dans la fonction `update` du programme.

## Voir aussi 

Pour en savoir plus sur la génération de nombres aléatoires en Elm et son utilisation, je vous recommande les ressources suivantes :

- [Random Numbers in Elm - Official Guide](https://guide.elm-lang.org/effects/random.html)
- [Elm Random package documentation](https://package.elm-lang.org/packages/elm/random/latest/)

---