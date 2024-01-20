---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

----
## Quoi & Pourquoi?
L'interpolation de chaînes en programmation permet d'insérer des valeurs de variables directement dans des chaînes de texte. Cette pratique facilite la construction et la manipulation de chaînes, en permettant d'écrire du code plus lisible et moins sujet à des erreurs.

## Comment faire:
Voici comment vous pouvez utiliser l'interpolation de chaînes dans Clojure. Clojure, actuellement en version 1.10.3, ne prend pas en charge l'interpolation de chaînes en interne, mais nous pouvons utiliser la fonction `format` pour obtenir un comportement similaire.

```clojure
(let [nom "John"]
  (format "Bonjour %s, comment ça va ?" nom))

; Sortie :
; "Bonjour John, comment ça va ?"
```

## Regard en profondeur:
Bien que Clojure lui-même n'ait pas d'interpolation de chaînes native, il y a des bibliothèques tierces, comme `clojure.string`, qui fournissent cette fonctionnalité. D'autre part, la fonction `format` est une alternative viable et généralement suffisante.

```clojure
(require '[clojure.string :as str])

(let [nom "John"]
  (str/replace "Bonjour %s, comment ça va ?" "%s" nom))

; Sortie :
; "Bonjour John, comment ça va ?"
```

C'est important à noter que l'interpolation de chaînes a été introduite dans d'autres langages, comme Ruby et JavaScript, pour rendre la manipulation de chaînes plus facile et le code plus lisible.

## Voir aussi:
Pour plus d'informations et d'exemples sur l'interpolation de chaînes, consultez ces sources :

2. [ClojureDocs - clojure.string](https://clojuredocs.org/clojure.string)