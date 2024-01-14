---
title:    "Clojure: Majuscule d'une chaîne de caractères"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous pourriez vous demander pourquoi il est important de capitaliser une chaîne de caractères en programmation Clojure. Eh bien, la capitalisation des chaînes de caractères peut être utile dans plusieurs cas, tels que l'affichage de noms ou de titres de manière correcte et l'utilisation de données dans des requêtes.

## Comment faire

Le code suivant est un exemple de fonction qui capitalise une chaîne de caractères en utilisant la fonction `capitalize` intégrée à Clojure :

```Clojure
(defn capitalize-str [str]
  (capitalize str))
```

En appelant cette fonction avec la chaîne de caractères "bonjour", nous obtenons "Bonjour" en sortie.

```Clojure
(capitalize-str "bonjour")
;; Output: Bonjour
```

Vous pouvez également utiliser la fonction `upper-case` pour capitaliser toute une phrase en majuscules :

```Clojure
(upper-case "j'aime les frites")
;; Output: J'AIME LES FRITES
```

## Plongée en profondeur

En Clojure, pour capitaliser une chaîne de caractères, la fonction `capitalize` utilise la règle de capitalisation du titre, où la première lettre de chaque mot est mise en majuscule. Cependant, vous pouvez utiliser une règle différente en utilisant la fonction `clojure.string/replace` et des expressions régulières.

Par exemple, si vous voulez que chaque mot commence par une minuscule, sauf pour le premier mot, vous pouvez utiliser cette expression régulière :

```Clojure
("(?=.[A-Z])|(?<=\\s).")
```

Cela signifie que la fonction `replace` va remplacer les première et deuxième lettres de chaque mot par elles-mêmes en majuscule, tandis que les autres lettres seront laissées telles quelles. Voici comment cela fonctionne :

```Clojure
(require '[clojure.string :as str])

(defn capitalize-first-letter [str]
  (str/replace str #"(?=.[A-Z])|(?<=\s)." #(str/upper-case %)))

(capitalize-first-letter "la capitale de la France est Paris")
;; Output: La Capitale De La France Est Paris
```

## Voir aussi

- Documentation officielle de la fonction `capitalize` : https://clojuredocs.org/clojure.core/capitalize
- Documentation officielle de la bibliothèque `clojure.string` : https://clojuredocs.org/clojure.string/replace