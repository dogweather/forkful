---
title:                "Conversion d'une chaîne en minuscules"
html_title:           "Elm: Conversion d'une chaîne en minuscules"
simple_title:         "Conversion d'une chaîne en minuscules"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La conversion d'une chaîne de caractères en minuscules est un processus qui consiste à transformer toutes les lettres d'une chaîne de caractères en lettres minuscules. Les programmeurs le font souvent pour normaliser les données saisies par les utilisateurs ou pour faciliter la comparaison de chaînes de caractères.

## Comment faire:
Voici un exemple de code en Elm qui illustre comment convertir une chaîne de caractères en minuscules:

```Elm
import String

input = "HELLO WORLD"
output = String.toLower input

```

Et voici le résultat:

```Elm
"hello world"
```

Comme vous pouvez le voir, la fonction `toLower` du module `String` est utilisée pour réaliser la conversion.

## Plongée en profondeur:
Il est intéressant de noter que la conversion de chaînes de caractères en minuscules n'était pas toujours aussi facile. Dans les anciennes versions de Elm, il n'y avait pas de fonction native pour réaliser cette tâche, les programmeurs devaient alors utiliser des astuces de programmation pour y parvenir.

De nos jours, il existe également d'autres alternatives pour réaliser cette conversion, comme l'utilisation de bibliothèques tierces. Mais la méthode la plus simple reste l'utilisation de la fonction native `toLower` du module `String`.

## À voir aussi:
- [Documentation officielle Elm sur la fonction `toLower`](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Une bibliothèque tierce pour gérer la conversion de chaînes de caractères en minuscules en Elm](https://package.elm-lang.org/packages/elm-community/string-extra/3.1.0/String-Extra#toLower)