---
title:                "Elm: Suppression de caractères correspondant à un modèle"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut être nécessaire, lors de la programmation en Elm, de supprimer des caractères correspondant à un modèle spécifique. Par exemple, vous pourriez vouloir supprimer tous les espaces d'une chaîne de caractères pour la nettoyer avant de l'utiliser dans une fonction ou une vue. Dans cet article, nous allons discuter de la façon de supprimer efficacement des caractères correspondant à un modèle en Elm.

## Comment Faire

Pour supprimer des caractères correspondant à un modèle en Elm, nous allons utiliser la fonction `String.replace` disponible dans le module `String`. Cette fonction prend deux arguments: le modèle à remplacer et la chaîne de caractères dans laquelle effectuer le remplacement. Voici un exemple de code montrant comment utiliser cette fonction pour supprimer tous les espaces d'une chaîne de caractères:

```Elm
import String

myString = "  Bonjour le monde !  "
cleanString = String.replace " " "" myString

-- Output: "Bonjourlemonde!"
```

Comme vous pouvez le voir, nous importons d'abord le module `String`, puis nous utilisons la fonction `String.replace` pour remplacer tous les espaces par une chaîne vide dans la variable `myString`. En évaluant la variable `cleanString`, nous obtenons la chaîne nettoyée sans espaces.

## Plongée Profonde

La fonction `String.replace` en Elm prend un troisième argument facultatif qui spécifie le nombre maximum de remplacements à effectuer. Si nous ne donnons pas cet argument, tous les caractères correspondant au modèle seront remplacés. Cependant, si nous voulons limiter le nombre de remplacements, nous pouvons utiliser cette troisième argument. Voici un exemple de code montrant comment supprimer seulement les trois premiers espaces d'une chaîne de caractères en utilisant `String.replace` avec un troisième argument:

```Elm
import String

myString = "  Bonjour le monde !  "
limitedString = String.replace " " "" myString 3

-- Output: "Bonjour le monde !"
```

En spécifiant que nous ne voulons que trois remplacements, le reste des espaces est laissé intacts.

## Voir Aussi

- Documentation de la fonction `String.replace` : https://package.elm-lang.org/packages/elm/core/latest/String#replace
- Tutoriel sur les fonctions de manipulation de chaînes en Elm : https://guide.elm-lang.org/appendix/strings.html
- Exemples de manipulation de chaînes en Elm : https://github.com/elm/projects/blob/master/examples/strings/src/Main.elm