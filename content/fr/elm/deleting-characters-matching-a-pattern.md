---
title:    "Elm: Suppression des caractères correspondant à un motif"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Pourquoi: 

Supprimer des caractères correspondant à un modèle peut être utile lors de la manipulation de chaînes de caractères en Elm. Cela peut faciliter la recherche et le remplacement de certaines parties d'une chaîne, ce qui peut être particulièrement utile lors de la manipulation de données provenant de sources externes.

## Comment faire:

Pour supprimer des caractères correspondant à un modèle en Elm, vous pouvez utiliser la fonction `String.filter` et la fonction `String.contains` pour vérifier si un caractère correspond au modèle spécifié. Voici un exemple de code qui montre comment supprimer tous les caractères non numériques d'une chaîne:

```Elm
import String exposing (contains, filter)

myString = "Ce951ll5e 1e2lm"
onlyNumbers = String.filter (\char -> contains char "0123456789") myString

-- Output: 9515212
```

Dans cet exemple, nous avons utilisé la fonction `filter` pour parcourir chaque caractère de la chaîne `myString` et la fonction `contains` pour vérifier si le caractère est un chiffre. Si c'est le cas, il sera inclus dans la chaîne `onlyNumbers`.

## Approfondissement:

En plus des fonctions `filter` et `contains`, il existe d'autres façons de supprimer des caractères correspondant à un modèle en Elm. Vous pouvez utiliser la fonction `String.dropWhile` pour supprimer tous les caractères jusqu'à ce qu'un certain modèle soit rencontré, ou utiliser la fonction `String.toUpper` pour convertir tous les caractères en majuscules. Vous pouvez également utiliser des expressions régulières avec la fonction `Regex.replace` pour supprimer ou remplacer des caractères correspondant à un modèle spécifique.

Il est important de noter que toutes ces fonctions renvoient une nouvelle chaîne de caractères au lieu de modifier la chaîne existante. Cela garantit que votre code reste immuable et évite toute confusion ou erreur lors de la manipulation des données.

## Voir aussi:

- Documentation officielle d'Elm sur la manipulation de chaînes de caractères: https://guide.elm-lang.org/strings/
- Tutoriel vidéo sur la manipulation de chaînes de caractères en Elm: https://www.youtube.com/watch?v=ob8UzQU6vMU
- Bibliothèque Elm pour l'utilisation d'expressions régulières: https://package.elm-lang.org/packages/elm-community/regex/latest/