---
title:    "Elm: Convertir une chaîne en minuscules"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des chaînes de caractères en Elm, vous pourriez avoir besoin de les convertir en lettres minuscules pour diverses raisons. La conversion de chaînes en minuscules peut faciliter la comparaison de chaînes, la recherche de sous-chaînes, ou simplement pour des raisons de formatage.

## Comment faire

Il existe une fonction native en Elm appelée `toLower` qui permet de convertir une chaîne en minuscules. Voici un exemple de son utilisation :

```Elm
import String

String.toLower "ELM EST AMUSANT"
```

La sortie de ce code sera "elm est amusant". Comme vous pouvez le voir, la fonction `toLower` prend simplement une chaîne en argument et la convertit en minuscules.

Vous pouvez également utiliser la fonction `map` pour appliquer cette conversion à une liste de chaînes. Voici un exemple :

```Elm
import String

listOfStrings = ["ELM", "EST", "AMUSANT"]

List.map String.toLower listOfStrings
```

La sortie de ce code sera une liste contenant les chaînes "elm", "est" et "amusant". La fonction `map` est utile si vous avez besoin de convertir plusieurs chaînes en même temps.

## Plongée en profondeur

Il est important de noter que la fonction `toLower` convertit une chaîne en utilisant les règles de séparation de mots Unicode. Cela signifie que certaines lettres peuvent ne pas être converties comme vous vous y attendez, en particulier pour les caractères non-latins.

Il est également possible de créer votre propre fonction de conversion pseudo-minuscule en utilisant `map` et la fonction `String.toCode` pour convertir chaque caractère individuel en code Unicode et ensuite le manipuler en utilisant des fonctions de comparaison. Cependant, cela devient rapidement complexe et il est préférable d'utiliser la fonction native `toLower` chaque fois que cela est possible.

## Voir aussi

- [Documentation officielle sur la manipulation des chaînes en Elm](https://guide.elm-lang.org/strings/) 
- [Liste des règles de séparation de mots Unicode](https://www.unicode.org/versions/Unicode10.0.0/ch03.pdf#page=127)
- [Documentation officielle String.toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)