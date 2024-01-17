---
title:                "Supprimer les caractères correspondant à un motif"
html_title:           "Elm: Supprimer les caractères correspondant à un motif"
simple_title:         "Supprimer les caractères correspondant à un motif"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire ?
Supprimer des caractères correspondant à un modèle est un moyen pour les programmeurs de manipuler des chaînes de caractères en supprimant des caractères qui correspondent à un motif spécifique. Par exemple, cela pourrait être utile pour supprimer tous les espaces de trop dans un texte pour le rendre plus lisible ou pour supprimer certains caractères spéciaux d'une chaîne avant de la stocker dans une base de données.

## Comment faire :
```Elm
deleteMatchingPattern : String → String → String
deleteMatchingPattern pattern string =
    String.filter (\c -> not (String.contains c pattern)) string
    
deleteSpaces : String
deleteSpaces =
    deleteMatchingPattern " " "Ce texte contient trop d'espaces."

-- Output: "Ce texte contienttropd'espaces."
```

## Plongez plus en profondeur :
La suppression de caractères correspondant à un modèle est souvent utilisée pour nettoyer ou formater des données avant de les traiter ou de les stocker. Cette technique est également couramment utilisée pour créer des masques de saisie sur des formulaires web ou pour supprimer des caractères spéciaux des messages saisis par les utilisateurs.

Il existe d'autres méthodes pour manipuler des chaînes de caractères, comme les expressions régulières, mais la suppression de caractères correspondant à un modèle est souvent choisie pour sa simplicité et sa rapidité d'exécution.

En termes de mise en œuvre, la fonction `filter` d'Elm est utilisée pour parcourir chaque caractère de la chaîne, et le caractère est supprimé s'il correspond au modèle spécifié.

## À voir également :
Pour en savoir plus sur les fonctions de manipulation de chaînes de caractères en Elm, consultez la [documentation officielle](https://guide.elm-lang.org/strings/) ou explorez la [source de la fonction `filter`](http://package.elm-lang.org/packages/elm-lang/core/latest/String#filter).