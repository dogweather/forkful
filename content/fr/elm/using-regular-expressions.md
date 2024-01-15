---
title:                "Utiliser les expressions régulières"
html_title:           "Elm: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous avez déjà essayé de chercher ou de manipuler des chaînes de caractères dans un langage de programmation, vous avez probablement réalisé à quel point cela peut être fastidieux et répétitif. Les expressions régulières, ou regex, sont un outil très utile pour rechercher, extraire et remplacer des motifs de chaînes de caractères de manière efficace et rapide.

## Comment faire

Dans Elm, les regex sont introduites avec la syntaxe `/pattern options/`, où le motif est entouré de deux barres obliques et les options sont facultatives et apparaissent après le motif. Voici un exemple de regex simple qui recherche les mots "hello" ou "salut" dans une chaîne de caractères :

```Elm
import Regex exposing (Regex)

input = "Bonjour à tous! Salut les amis!"

pattern = Regex.regex "/(hello|salut)/"

result = Regex.find pattern input

-- Le résultat sera [|(Just (1,5, "salut"))|]
```

Comme vous pouvez le voir, la fonction `Regex.find` renvoie le résultat sous forme de liste contenant des tuples avec les correspondances trouvées. Dans cet exemple, nous avons trouvé le mot "salut" à partir de l'index 1 (en raison de l'option "g" pour une recherche globale).

Les regex peuvent également être utilisées pour capturer des sous-groupes de motifs en utilisant des parenthèses. Voici un exemple où nous cherchons à capturer un numéro de téléphone avec un code de pays à partir d'une chaîne de caractères :

```Elm
input = "Mon numéro de téléphone est +33 6 12 34 56 78"

pattern = Regex.regex "/\\+(\\d{2}) (\\d{1,2} ?){5}/"

result = Regex.find pattern input

-- Le résultat sera [|(Just (21,24, "33")),(Just (25,27, "6")),(Just (28,32, "12")),(Just (33,35, "34")),(Just (36,38, "56")),(Just (39,41, "78"))|]
```

Pour plus d'exemples et d'informations sur les options disponibles en Elm pour les regex, consultez la documentation officielle.

## Plongée en profondeur

Les regex peuvent sembler déroutantes au début en raison de leur syntaxe cryptique et de leur utilisation de caractères spéciaux pour définir des motifs. Cependant, une fois que vous avez compris les bases, elles peuvent être un outil puissant pour automatiser des tâches de manipulation de chaînes de caractères. Voici quelques conseils à garder en tête lors de l'utilisation de regex en Elm :

- Utilisez des caractères d'échappement pour éviter toute confusion avec les caractères spéciaux.
- Utilisez des options telles que "i" pour une recherche insensible à la casse et "m" pour rechercher sur plusieurs lignes.
- Expérimentez avec différents sites en ligne pour tester vos regex.
- N'oubliez pas que les regex peuvent être coûteuses en termes de performances, essayez donc d'optimiser votre motif si possible.

## Voir aussi

- [Documentation officielle Elm pour les regex](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Un guide complet pour apprendre les regex](https://regexone.com/)
- [Un outil interactif en ligne pour tester les regex en temps réel](https://regex101.com/)