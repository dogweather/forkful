---
title:                "Clojure: Extraction de sous-chaînes"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi
Vous vous demandez pourquoi certains programmeurs choisissent d'extraire des sous-chaînes dans leurs codes ? Eh bien, cela peut être utile dans de nombreux cas, notamment pour traiter des données, manipuler des chaînes de caractères ou créer des algorithmes efficaces.

## Comment Faire
Dans Clojure, il existe plusieurs façons d'extraire des sous-chaînes. Vous pouvez utiliser la fonction `subs`, qui prend deux arguments : la chaîne de caractères et les indices de début et de fin pour la sous-chaîne souhaitée.

```Clojure
(def chaine "Bonjour le monde")

(subs chaine 0 7) ; renvoie "Bonjour"
(subs chaine 8 11) ; renvoie "le"
```

Vous pouvez également utiliser l'opérateur `get` avec une chaîne de caractères et un index pour obtenir un caractère spécifique à partir de la chaîne.

```Clojure
(get chaine 0) ; renvoie le premier caractère "B"
(get chaine 10) ; renvoie le dernier caractère "e"
```

Enfin, vous pouvez utiliser la fonction `split` pour diviser une chaîne en sous-chaînes à l'aide d'un séparateur spécifié.

```Clojure
(split "Bonjour,le,monde" #",") ; renvoie ["Bonjour" "le" "monde"]
```

## Plongée Profonde
Il est important de noter que les indices utilisés pour extraire des sous-chaînes en Clojure sont inclusifs pour le début et exclusifs pour la fin. Par exemple, pour extraire les trois premiers caractères d'une chaîne, vous devriez utiliser les indices 0 et 3, plutôt que 1 et 3.

De plus, vous pouvez également utiliser des fonctions de manipulation de chaînes de caractères plus avancées, telles que `ifind`, `clojure.string/index-of`, et `clojure.string/replace` pour extraire et manipuler des sous-chaînes dans vos codes.

## Voir Aussi
- Documentation officielle de Clojure sur les sous-chaînes : https://clojuredocs.org/clojure.core/subs
- Guide de référence rapide sur les fonctions de manipulation de chaînes de caractères en Clojure : https://clojuredocs.org/clojure.string/index
- Exemples pratiques d'extractation de sous-chaînes en Clojure : https://dzone.com/articles/string-functions-in-clojure-a-how-to-guide