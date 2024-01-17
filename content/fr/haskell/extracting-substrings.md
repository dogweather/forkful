---
title:                "Extraction de sous-chaines"
html_title:           "Haskell: Extraction de sous-chaines"
simple_title:         "Extraction de sous-chaines"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'extraction de sous-chaînes est une pratique courante en programmation qui consiste à découper une chaîne de caractères en portions plus petites, ou sous-chaînes, pour faciliter leur utilisation dans le code. Les programmeurs le font souvent lorsqu'ils ont besoin de manipuler ou de comparer des parties spécifiques d'une chaîne.

## Comment faire:

Voici trois exemples de code en Haskell pour extraire des sous-chaînes à partir d'une chaîne donnée, ainsi que leur sortie respective:

```Haskell
-- Le premier argument est l'indice du début de la sous-chaîne
-- Le deuxième argument est l'indice de la fin de la sous-chaîne
drop 3 (take 6 "Bonjour tout le monde!")
-- "jour"

-- Le premier argument est le nombre de caractères à supprimer au début de la chaîne
-- Le deuxième argument est la chaîne où effectuer l'extraction
drop 3 "Bonjour"
-- "jour"

-- Le premier argument est l'indice du début de la sous-chaîne
-- Le deuxième argument est la longueur de la sous-chaîne
take 5 "Hello world"
-- "Hello"
```

## Deep Dive:

L'extraction de sous-chaînes existe depuis les débuts de la programmation, mais la façon dont elle est mise en œuvre peut varier selon les langages. Par exemple, en Python, on utilise la syntaxe `string[n:m]` pour extraire une sous-chaîne qui commence à l'indice n et se termine à l'indice m. En Haskell, on utilise plutôt les fonctions `drop` et `take` comme dans les exemples ci-dessus.

Il existe également des alternatives à l'extraction de sous-chaînes, comme l'utilisation de regex (expressions régulières) pour trouver et extraire des motifs spécifiques dans une chaîne. Cependant, l'utilisation de fonctions de base comme `drop` et `take` est souvent plus simple et plus efficace pour des besoins ponctuels.

## See Also:

Voici quelques liens utiles pour en savoir plus sur l'extraction de sous-chaînes en Haskell:

- La documentation officielle sur les fonctions `drop` et `take`: https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#v:drop
- Un tutoriel sur l'utilisation des expressions régulières en Haskell: https://wiki.haskell.org/Regular_expressions
- Un article sur les bonnes pratiques pour manipuler les chaînes de caractères en Haskell: https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_String_Manipulation_in_Haskell