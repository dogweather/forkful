---
title:    "Elm: Extraction de sous-chaînes"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est souvent nécessaire de manipuler des chaînes de caractères. Cela peut inclure la création de sous-chaînes ou l'extraction de parties spécifiques d'une chaîne plus grande. C'est là qu'entre en jeu l'extraction de sous-chaînes en Elm, une fonction utile qui permet de simplifier ce processus.

## Comment faire

L'extraction de sous-chaînes en Elm se fait à l'aide de la fonction `String.slice`, qui prend en paramètre une chaîne de caractères, ainsi que deux indices représentant le début et la fin de la sous-chaîne à extraire. Voici un exemple de code :

```Elm
myString = "Bonjour tout le monde"
substring = String.slice 8 12 myString
```

Dans cet exemple, nous extrayons la sous-chaîne "tout" de `myString`, en utilisant les indices 8 et 12 pour délimiter la partie que nous voulons extraire. Lorsque nous imprimons `substring` sur la console, nous obtenons le résultat suivant :

```
"tout"
```

Vous pouvez également utiliser un seul paramètre pour la fonction `String.slice` afin de spécifier uniquement le début de la sous-chaîne. Dans ce cas, la fonction prendra le reste de la chaîne à partir de cet index. Par exemple :

```Elm
myString = "Bonjour tout le monde"
substring = String.slice 8 myString
```

Dans cet exemple, nous extrayons la sous-chaîne "tout le monde". La console affichera :

```
"tout le monde"
```

## Plongée en profondeur

Il est également possible d'utiliser la fonction `String.slice` pour extraire des parties spécifiques en utilisant des valeurs négatives pour les indices. Cela nous permet de compter en partant de la fin de la chaîne de caractères. Par exemple, si nous voulons extraire les deux derniers mots de `myString`, nous pouvons utiliser les indices -2 et -1 :

```Elm
myString = "Bonjour tout le monde"
substring = String.slice -6 -1 myString
```

La console affichera alors :

```
"le monde"
```

Il est également possible d'utiliser la fonction avec des indices en dehors de la plage de la chaîne de caractères. Dans ce cas, la fonction renverra simplement une chaîne vide. Par exemple :

```Elm
myString = "Bonjour tout le monde"
substring = String.slice 20 25 myString
```

La console affichera :

```
""
```

## Voir aussi

- [Documentation officielle d'Elm sur l'extraction de sous-chaînes](https://elm-lang.org/docs/string#slice)
- [Article sur les chaînes de caractères en Elm](https://www.brianthicks.com/post/2016/08/05/a-journey-through-the-bowels-of-brian-part-2-strings/)