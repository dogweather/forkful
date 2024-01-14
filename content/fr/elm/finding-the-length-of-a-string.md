---
title:    "Elm: Trouver la longueur d'une chaîne de caractères"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Il y a plusieurs raisons pour lesquelles vous pourriez vouloir trouver la longueur d'une chaîne en programmation Elm. Par exemple, cela peut être utile pour valider les entrées de l'utilisateur ou pour manipuler des données dans votre programme.

# Comment faire

Pour trouver la longueur d'une chaîne en Elm, vous pouvez utiliser la fonction `String.length`. Voici un exemple de code avec un input et un output :

```Elm
chaine = "Bonjour!"
longueur = String.length chaine
```

Dans cet exemple, la variable `longueur` aura pour valeur 8 car il y a 8 caractères dans la chaîne "Bonjour!". Vous pouvez également combiner cette fonction avec d'autres fonctions de manipulation de chaînes pour obtenir des résultats plus complexes.

# Plongée en profondeur

La fonction `String.length` utilise en fait un concept appelé le comptage de graphèmes, qui est une méthode de mesure de la longueur des caractères d'une chaîne en prenant en compte les caractères accentués et les lettres composées. En utilisant cette fonction, vous pouvez vous assurer que votre programme prend en compte toutes les nuances de la langue française.

# Voir aussi

- Documentation officielle sur la fonction `String.length` : https://package.elm-lang.org/packages/elm-lang/core/latest/String#length
- Un autre article sur les manipulations de chaînes en Elm : https://www.codementor.io/@elm/style-guide-for-elm-string-manipulation-x317gv84q