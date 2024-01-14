---
title:    "Clojure: Utiliser des expressions régulières"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont très utiles lorsqu'il s'agit de trouver des correspondances ou de filtrer des données dans du texte. Elles permettent une recherche précise et rapide, ainsi qu'une manipulation efficace des chaînes de caractères. Dans le domaine de la programmation, les expressions régulières sont un outil puissant pour traiter et analyser des données.

## Comment faire

Pour utiliser les expressions régulières en Clojure, il faut d'abord importer la bibliothèque `clojure.string` qui contient les fonctions nécessaires. Ensuite, on peut utiliser la fonction `re-find` pour trouver des correspondances dans une chaîne de caractères en utilisant une expression régulière.

```
(ns mon-projet.core
  (:require [clojure.string :as str]))

(str/re-find #"a.b" "abc") ;; renvoie "abc"
(str/re-find #"a\\sb" "a b") ;; renvoie "a b"
(str/re-find #"a\.b" "a.b") ;; renvoie "a.b"
(str/re-find #"a.*b" "abcdefg") ;; renvoie "abcdefg"
```

Dans l'exemple ci-dessus, le `#` indique qu'il s'agit d'une expression régulière, puis on met l'expression entre guillemets `""`. Les `.` et `*` sont des opérateurs spéciaux qui permettent de trouver toutes les correspondances possibles. De plus, on peut utiliser certains caractères spéciaux tels que `\s` pour représenter des espaces et `\\` pour échapper un caractère spécial.

## Plongée en profondeur

Les expressions régulières peuvent être utilisées pour une multitude de cas d'utilisation, tels que la validation de formulaires, la manipulation de données, ou encore la recherche et le remplacement de texte. On peut également utiliser des "groupes" dans les expressions régulières pour capturer une partie spécifique du texte.

```
(ns mon-projet.core
  (:require [clojure.string :as str]))

(def texte "Bonjour à tous !")

(str/re-find #"Bonjour à (\w+) !" texte) ;; renvoie ["Bonjour à tous !" "tous"]
```

Dans cet exemple, le `(\w+)` permet de capturer le "groupe" entre les parenthèses et ainsi récupérer uniquement le mot "tous". Cela peut être utile pour effectuer des opérations spécifiques sur une partie du texte.

## Voir aussi

- [Documentation officielle de Clojure sur les expressions régulières](https://clojure.org/reference/regular_expressions)
- [Tutoriel sur le site Exercism pour pratiquer les expressions régulières en Clojure](https://exercism.io/tracks/clojure/exercises/regex/custom-set)
- [Référence complète des opérateurs et des caractères spéciaux en expressions régulières](https://www.regular-expressions.info/reference.html)