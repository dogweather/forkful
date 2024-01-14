---
title:    "Gleam: Extraction de sous-chaînes"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes de caractères est une tâche courante en programmation, que ce soit pour manipuler des données ou pour effectuer des opérations sur des chaînes de caractères. Dans cet article, nous allons explorer comment effectuer cette opération en utilisant le langage de programmation Gleam.

## Comment faire

Pour extraire une sous-chaîne de caractères en utilisant Gleam, nous allons utiliser la fonction `substring()` et spécifier l'index de début et de fin de la sous-chaîne.

```Gleam
let chaine = "Bonjour tout le monde"
let sous_chaine = substring(chaine, 8, 12)
// sous_chaine = "tout"
```

La fonction `substring()` prend trois paramètres : la chaîne de caractères d'origine, l'index de début de la sous-chaîne et l'index de fin de la sous-chaîne. Il est important de noter que l'indexage des chaînes de caractères en Gleam commence à partir de zéro.

Pour extraire une sous-chaîne à partir de la fin de la chaîne, nous pouvons utiliser l'index négatif. Par exemple, pour obtenir les 3 derniers caractères de la chaine d'origine, nous pouvons utiliser l'index `-3` pour l'index de début de la sous-chaîne.

```Gleam
let chaine = "Bonjour tout le monde"
let sous_chaine = substring(chaine, -3, -1)
// sous_chaine = "nd"
```

Nous pouvons également utiliser l'opérateur de tranche (`[start..end]`) pour extraire une sous-chaîne de caractères.

```Gleam
let chaine = "Bonjour tout le monde"
let sous_chaine = chaine[8..12]
// sous_chaine = "tout"
```

En utilisant l'opérateur de tranche, nous pouvons également spécifier un pas, ce qui nous permet d'extraire chaque x caractères de la chaîne de caractères d'origine.

```Gleam
let chaine = "Bonjour tout le monde"
let sous_chaine = chaine[0..-1:2]
// sous_chaine = "Bjnru o eoe"
```

## Plongée en profondeur

Il est important de noter que la fonction `substring()` utilise un algorithme qui copie une partie de la chaîne de caractères d'origine dans une nouvelle chaîne de caractères. Cela signifie que si la chaîne de caractères d'origine est très grande, l'algorithme devra allouer beaucoup de mémoire pour stocker la sous-chaîne.

Il y a également des opérations plus avancées que nous pouvons effectuer en utilisant l'opérateur de tranche. Par exemple, nous pouvons utiliser l'opérateur de tranche pour remplacer une sous-chaîne spécifique dans une chaîne de caractères.

## Voir aussi

- La documentation officielle de Gleam sur l'extraction de sous-chaînes : [lien vers la documentation officielle](https://gleam.run/documentation/#string-substring)
- Un exemple de projet utilisant Gleam pour extraire des sous-chaînes : [lien vers le projet](https://github.com/username/exemple-gleam-sous-chaines)
- Une discussion approfondie sur les performances de l'extraction de sous-chaînes en Gleam : [lien vers l'article de blog](https://mon-blog.com/deep-dive-extraction-sous-chaines-gleam)