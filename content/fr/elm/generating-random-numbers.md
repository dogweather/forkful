---
title:    "Elm: Génération de nombres aléatoires"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi générer des nombres aléatoires en Elm ?

Générer des nombres aléatoires est un aspect important de la programmation, car cela permet d'introduire de l'aléatoire dans les programmes. Cela peut être utile pour créer des jeux, des simulations ou des tests aléatoires. En Elm, il existe plusieurs façons de générer des nombres aléatoires, ce qui le rend très pratique pour différents cas d'utilisation.

## Comment le faire en Elm ?

Il existe deux principaux moyens de générer des nombres aléatoires en Elm : en utilisant la fonction `Random.generate` ou en utilisant un générateur défini par l'utilisateur. Voici un exemple de chaque méthode :

```
Elm Random.Generate :

init : ( List Line, Int )
init =
  ( [], 0 ) -- Initialise la liste et la seed

generateRandomNumber : Generator Int
generateRandomNumber =
  Generator.int 1 10 -- Génère un nombre compris entre 1 et 10

newRandomNumber : Int -> Cmd Msg
newRandomNumber oldNumber =
  Random.generate NewNumber generateRandomNumber
  -- Génère un nouveau nombre et envoie un message avec le nouveau nombre

```

```
Générateur défini par l'utilisateur :

type alias Generator a =
  Seed -> ( a, Seed ) -- Prend en entrée une seed et renvoie une valeur aléatoire ainsi qu'une nouvelle seed

newGenerator : Int -> Generator Int
newGenerator max seed =
  ( seed + 1, (seed * 32573 + 10001) % max ) -- Définit un générateur prenant en compte une valeur max et une seed

getRandomNumber : Int -> Cmd Msg
getRandomNumber max =
  let
    seed = 123 -- Valeur de départ pour la seed
    result = newGenerator max seed
  in
    Random.generate NewNumber result -- Génère un nouveau nombre avec le générateur défini par l'utilisateur

```

Dans ces exemples, nous utilisons les fonctions `Random.generate` et `Generator` pour générer des nombres aléatoires. Dans le premier exemple, nous utilisons la fonction `int` qui génère un nombre entier compris entre une valeur minimale et une valeur maximale données. Dans le deuxième exemple, nous définissons notre propre générateur en utilisant une combinaison de valeurs et de calculs pour obtenir une valeur aléatoire dans une plage définie.

## Plongée en profondeur

Lorsque vous utilisez la fonction `Random.generate`, il est important de noter que la seed donnée en entrée doit être différente à chaque fois pour obtenir un résultat différent. Dans le cas contraire, la même valeur aléatoire sera générée à chaque fois. Il est donc recommandé d'utiliser la fonction `Time.now` pour générer une seed unique pour chaque utilisation de la fonction `Random.generate`.

De plus, il est également possible de combiner plusieurs générateurs en utilisant la fonction `map2` ou la fonction `andThen` pour obtenir des valeurs aléatoires plus complexes ou en utilisant des générateurs spécifiques pour différents types de données, tels que les nombres entiers, les nombres flottants ou les booléens.

# Voir aussi

Voici quelques liens utiles pour en savoir plus sur la génération de nombres aléatoires en Elm :

- [Documentation officielle Elm : Random](https://package.elm-lang.org/packages/elm/random/latest/)
- [Article sur la génération de nombres aléatoires en Elm](https://www.techtalkshq.com/generating-random-numbers-in-elm/)
- [Vidéo explicative sur la génération de nombres aléatoires en Elm](https://www.youtube.com/watch?v=mk12Dfu_o8c)