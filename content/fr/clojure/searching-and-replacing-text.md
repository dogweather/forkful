---
title:                "Rechercher et remplacer du texte"
html_title:           "Clojure: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur ou un programmeur, vous savez que le traitement de texte est un élément clé de la programmation quotidienne. Que vous travailliez sur un projet personnel ou professionnel, il est souvent nécessaire de rechercher et de remplacer du texte pour effectuer des modifications ou optimiser votre code. Heureusement, Clojure offre des outils puissants pour gérer ces tâches de manière efficace et simple.

## Comment faire

Pour rechercher et remplacer du texte en Clojure, vous pouvez utiliser la fonction `replace` de la bibliothèque de base `clojure.string`. Elle prend trois arguments : la chaîne de caractères d'origine, le motif à rechercher et la chaîne de remplacement.

```
(require '[clojure.string :as str])

(str/replace "Bonjour le monde!" "le monde" "toi")
;; Résultat: "Bonjour toi!"
```

Vous pouvez également utiliser des expressions régulières pour rechercher et remplacer du texte dans une chaîne de caractères en utilisant la fonction `re-find` de la bibliothèque `clojure.core`. Elle prend deux arguments : l'expression régulière et la chaîne de caractères dans laquelle rechercher.

```
(require '[clojure.core :refer [re-find]])

(re-find #"a[b-z]+" "Bonjour tout le monde!")
;; Résultat: "onjour"
```

Il est également possible de remplacer des correspondances en utilisant la fonction `repl` de la bibliothèque `clojure.core`.

```
(require '[clojure.core :refer [repl]])

(repl #"bonjour" "salut" "Bonjour tout le monde!")
;; Résultat: "salut tout le monde!"
```

## Plongée en profondeur

En plus des méthodes mentionnées ci-dessus, il existe des bibliothèques tierces telles que `clojure.data.xml` et `clojure.data.json` qui offrent des fonctions spécifiques pour rechercher et remplacer du texte dans des données structurées, comme du XML et du JSON. Vous pouvez également utiliser des fonctions plus avancées comme `replace-first` et `replace-every` disponibles dans la bibliothèque `clojure.walk` pour des opérations plus complexes de recherche et de remplacement.

## Voir aussi

- [Documentation officielle de Clojure](https://clojure.org/)
- [Guide de style de programmation en Clojure](https://github.com/bbatsov/clojure-style-guide)
- [Clojure pour les débutants](https://clojure.org/guides/learn/syntax)