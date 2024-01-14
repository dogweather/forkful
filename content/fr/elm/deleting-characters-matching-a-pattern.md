---
title:    "Elm: Supprimer les caractères correspondants à un modèle"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut être utile de supprimer des caractères correspondants à un motif lors de la programmation en Elm pour plusieurs raisons. Cela peut simplifier le traitement de données en éliminant des informations inutiles, ou permettre de réaliser des opérations plus complexes sur une chaîne de caractères.

## Comment faire

Pour supprimer des caractères correspondants à un motif en Elm, nous pouvons utiliser la fonction `replace` de la bibliothèque `String`. Elle prend en paramètre le motif à remplacer ainsi que la chaîne de caractères à traiter. Voici un exemple de code :

```Elm
import String

phrase = "Bonjour tout le monde!"
nouvellePhrase = String.replace "tout le monde" "" phrase

-- Resultat: "Bonjour !"
```

Dans ce cas, la fonction a supprimé les caractères correspondants au motif "tout le monde" de la phrase d'origine, laissant ainsi une chaîne plus courte et plus concise en sortie.

## Analyse approfondie

Il est important de noter que la fonction `replace` de la bibliothèque `String` ne supprime que les caractères correspondants exactement au motif spécifié. Cela signifie que les caractères doivent être identiques, y compris la casse des lettres. De plus, il est également possible de supprimer plusieurs occurrences du motif en spécifiant un troisième paramètre optionnel correspondant au nombre d'occurrences à remplacer.

Dans certains cas, il peut être utile de supprimer des caractères correspondant à un motif non seulement dans une chaîne de caractères, mais aussi dans une liste de chaînes. Pour ce faire, nous pouvons utiliser la fonction `map` pour appliquer la fonction `replace` à chaque élément de la liste.

## Voir aussi

Pour plus d'informations sur la fonction `replace` et d'autres fonctionnalités de la bibliothèque `String` en Elm, vous pouvez consulter la documentation officielle sur [le site de la communauté Elm](https://elm-lang.org/docs).

Pour en savoir plus sur la programmation en Elm, vous pouvez également consulter les ressources suivantes :

- [Le guide officiel Elm](https://guide.elm-lang.org/)
- [La chaîne YouTube Elm France](https://www.youtube.com/channel/UCVqr5pepwpgFpiblbJwGSgQ)
- [La bibliothèque open-source Elm](https://elm-lang.org/)

N'hésitez pas à utiliser la fonction `replace` et à explorer toutes les possibilités qu'elle offre pour simplifier vos opérations sur les chaînes de caractères en Elm !