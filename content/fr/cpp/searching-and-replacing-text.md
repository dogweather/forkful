---
title:                "Recherche et remplacement de textes"
html_title:           "C++: Recherche et remplacement de textes"
simple_title:         "Recherche et remplacement de textes"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

"Rechercher et remplacer du texte" est une tâche courante pour les programmeurs, qui consiste à modifier du texte dans un programme en remplaçant certaines occurrences par d'autres. Nous le faisons pour corriger des erreurs, mettre à jour du code obsolète ou pour faire des modifications en vrac.

## Comment faire:

Voici quelques exemples de code en ```C++``` pour montrer deux façons différentes de rechercher et remplacer du texte dans une chaîne de caractères:

1. Utilisation de la fonction "find" pour localiser la position d'une occurrence spécifique, puis utilisation de la fonction "replace" pour la remplacer:
```C++
string str = "Bonjour le monde!";
int pos = str.find("monde");
str.replace(pos, 5, "univers");
```
Output: "Bonjour l'univers!"

2. Utilisation de la librairie `<algorithm>` pour utiliser la fonction "replace" sur l'ensemble de la chaîne de caractères:
```C++
string str = "Bonjour le monde!";
replace(str.begin(), str.end(), 'o', 'a');
```
Output: "Banjaur le mande!"

## Plongée en profondeur:

Historiquement, la recherche et le remplacement de texte étaient des opérations manuelles et fastidieuses pour les programmeurs. Avec l'avènement des éditeurs de texte et des logiciels de développement, cette tâche est devenue beaucoup plus rapide et plus efficace. Les alternatives à la recherche et au remplacement de texte incluent l'utilisation de macros et d'expressions régulières pour des modifications plus complexes. L'implémentation dépendra du langage de programmation utilisé.

## Voir aussi:

- [Documentation officielle de la librairie `<algorithm>` en C++](https://en.cppreference.com/w/cpp/algorithm)
- [Exemples de remplacement de texte en C++ sur GeeksforGeeks](https://www.geeksforgeeks.org/replace-a-string-in-cplusplus/)