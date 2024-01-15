---
title:                "Génération de nombres aléatoires"
html_title:           "Elm: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Êtes-vous prêt pour un peu de diversion dans votre code Elm ? Nous allons parler de quelque chose de vraiment amusant - la génération de nombres aléatoires ! Non seulement cela ajoute un aspect de jeu à votre application, mais cela peut également être utile pour simuler des données ou créer des tests automatisés avec des données variables.

## Comment faire

La génération de nombres aléatoires en Elm est assez simple grâce à la fonction `Random.generate`. Voici un exemple de code pour générer un nombre aléatoire entre 1 et 10 :

```Elm
Random.generate (Random.int 1 10) -- renvoie un entier entre 1 et 10
```

Le résultat sera stocké dans un `Cmd` qui pourra être utilisé pour mettre à jour votre modèle. Voici un exemple de mise à jour de modèle utilisant la génération de nombre aléatoire :

```Elm
update msg model =
    case msg of
        RandomNumber number ->
            { model | random = number }

        GenerateRandom ->
            ( model, Random.generate (Random.int 1 10) RandomNumber )
```

Ici, `RandomNumber` est une action que vous pouvez définir pour mettre à jour votre modèle avec le nombre aléatoire généré. Et `GenerateRandom` est une action qui peut être déclenchée par un bouton ou une autre interaction utilisateur pour lancer la génération de nombre aléatoire. Enfin, dans votre vue, vous pouvez afficher le résultat de la génération de nombre aléatoire en utilisant le champ `random` dans votre modèle.

## Plongée en profondeur

Maintenant que vous savez comment générer des nombres aléatoires, vous pourriez vous demander : "Qu'en est-il de la graine ou de la gestion des probabilités ?" Eh bien, ne vous inquiétez pas, vous pouvez également spécifier une graine pour la génération de nombre aléatoire en utilisant la fonction `Random.generateWithSeed`. De plus, vous pouvez également utiliser les fonctions `andMap` et `map2` pour combiner plusieurs générateurs de nombre aléatoire pour créer des résultats plus complexes. N'hésitez pas à explorer toutes les options que la bibliothèque `Random` d'Elm a à offrir !

## Voir aussi

- Documentation officielle Elm sur la génération de nombres aléatoires : https://package.elm-lang.org/packages/elm/random/latest/
- Exemples d'utilisation de la génération de nombres aléatoires en Elm : https://elmprogramming.com/random-numbers-elm.html
- Blog sur les avantages de la génération de nombres aléatoires en Elm : https://elm.brianthicks.com/random-number-generation/