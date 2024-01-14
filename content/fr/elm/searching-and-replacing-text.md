---
title:                "Elm: Recherche et remplacement de texte"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Elm et que vous souhaitez gérer efficacement de grands ensembles de données, la recherche et le remplacement de texte sont des compétences essentielles à maîtriser. En utilisant les bonnes méthodes, vous pouvez économiser beaucoup de temps et d'efforts dans la manipulation de votre code.

## Comment faire

Pour effectuer une recherche et un remplacement de texte en Elm, nous utiliserons la fonction `replace` du module `String`. Voici un exemple simple de code qui remplace une chaîne de caractères par une autre dans une liste :

```Elm
liste = ["Bonjour", "Salut", "Hello"]
nouvelleListe = List.map (String.replace "Bonjour" "Hi") liste

-- output : ["Hi", "Salut", "Hello"]
```

En utilisant la fonction `map`, nous pouvons appliquer la fonction de remplacement à chaque élément de la liste et créer une nouvelle liste avec les modifications.

Il est également possible d'utiliser des expressions régulières pour effectuer des remplacements plus complexes. Voici un exemple de code qui supprime tous les espaces dans une chaîne de caractères :

```Elm
chaine = "La vie est belle"
nouvelleChaine = String.replaceRegex "\\s+" "" chaine

-- output : "Lavieestbelle"
```

Nous utilisons la fonction `replaceRegex` pour rechercher tous les espaces (représentés par la chaîne de caractères `"\\s+"`) et les remplacer par une chaîne vide. Cela nous donne une chaîne sans espace.

## Plongée en profondeur

La fonction `replace` utilise en fait une expression régulière pour rechercher et remplacer le texte. Cette expression est créée en interne à partir des arguments que nous passons à la fonction.

Si vous souhaitez utiliser une expression régulière personnalisée pour remplacer du texte, vous pouvez utiliser la fonction `replaceRegex` directement. Vous pouvez également consulter la documentation de la bibliothèque `elm/parser` pour en apprendre davantage sur les expressions régulières en Elm.

## Voir aussi

- Documentation officielle pour la fonction `replace` : https://package.elm-lang.org/packages/elm/core/latest/String#replace
- Documentation officielle pour le module `String` : https://package.elm-lang.org/packages/elm/core/latest/String
- Documentation de la bibliothèque `elm/parser` : https://package.elm-lang.org/packages/elm/parser/latest/