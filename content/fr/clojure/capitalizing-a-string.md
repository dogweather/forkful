---
title:                "Mettre une chaîne de caractères en majuscules"
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Majusculer une chaîne, c'est transformer toutes les lettres en majuscules. Les programmeurs l'utilisent pour normaliser des textes en vue de comparaisons ou d'affichages uniformes.

## How to:
En Clojure, on majuscule avec `clojure.string/upper-case` :

```clojure
(require '[clojure.string :as str])

(str/upper-case "Salut Clojure !")
;; Résultat: "SALUT CLOJURE !"
```

## Deep Dive
Dans le monde de la programmation, majusculer une chaîne est un concept ancien. Avant Unicode, travailler avec des ASCII était plus simple. Maintenant, la fonction `clojure.string/upper-case` gère Unicode, donc c'est plus complexe sous le capot.

Avant `clojure.string/upper-case`, on aurait pu utiliser `map` et `char` pour faire une fonction soi-même :

```clojure
(defn to-upper-case [s]
  (apply str (map #(Character/toUpperCase %) s)))
```
Cependant, cette méthode brute ne gère pas les spécificités de chaque langue. Le passage à `clojure.string/upper-case` offre une abstraction qui gère ces détails.

Alternativement, on peut dire que majusculer est contextuel. Par exemple, pour le turc, la lettre "i" majuscule n'est pas "I" avec un point. Ces subtilités linguistiques rendent `upper-case` plus complexe qu'il paraît.

## See Also
- La [documentation officielle de Clojure](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/upper-case), pour le détail des fonctions de chaîne.
- La [page de Unicode](http://www.unicode.org) pour comprendre comment les majuscules fonctionnent dans différents alphabets.
- [Wikipedia sur ASCII](https://en.wikipedia.org/wiki/ASCII) pour l'histoire du codage de caractères avant Unicode.
