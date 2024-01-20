---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qu'est-ce que & Pourquoi?

Convertir une chaîne en minuscules signifie transformer toutes les lettres majuscules en lettres minuscules. Les programmeurs le font pour rendre l'information saisie par l'utilisateur uniforme et faciliter les comparaisons textuelles.

## Comment faire:

En Elm, vous pouvez utiliser la fonction `String.toLower` pour convertir une chaîne en minuscules. Voici comment ça marche:

```Elm
import String

output = String.toLower "Bonjour MONDE!"
```
Ainsi, `output` contiendra `bonjour monde!`

## Plongée profonde:

Historiquement, les ordinateurs n'étaient pas capables de faire la distinction entre les majuscules et les minuscules. Mais avec l'évolution de la technologie, les programmeurs ont développé des fonctions comme `String.toLower` pour convertir les chaînes en minuscules, facilitant les comparaisons.

Alternativement, on peut écrire une fonction custom en utilisant un map sur chaque caractère de la chaîne, mais `String.toLower` est généralement plus efficace et rapide.

En termes de mise en œuvre, `String.toLower` fonctionne en parcourant la chaîne de caractères et en remplaçant chaque lettre majuscule par sa version minuscule. C'est un processus simple et rapide qui a de nombreux usages, en particulier lorsqu'il s'agit de manipuler des données saisies par l'utilisateur.

## Voir aussi:

Pour plus d'informations sur les chaines de caractères en Elm, consultez la documentation officielle :
- [Documentation Elm pour String](https://package.elm-lang.org/packages/elm/core/latest/String)