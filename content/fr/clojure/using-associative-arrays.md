---
title:                "Utilisation des tableaux associatifs"
date:                  2024-01-30T19:10:11.703608-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des tableaux associatifs"

category:             "Clojure"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les tableaux associatifs, ou hash maps, dans Clojure vous permettent de stocker et de récupérer des données avec des paires clé-valeur. Ils sont un choix privilégié pour gérer des données structurées, facilitant l'accès rapide à des éléments spécifiques sans avoir à parcourir une liste.

## Comment faire :

Dans Clojure, créer et manipuler des tableaux associatifs (hash maps) est simple. Plongeons dans quelques exemples.

Pour créer une hash map :

```clojure
(def my-map {:name "Alex" :age 30})
```

Vous pouvez récupérer une valeur en spécifiant sa clé :

```clojure
(get my-map :name)
;; "Alex"
```
Ou, de manière plus idiomatique, vous pouvez utiliser la clé comme une fonction :

```clojure
(:name my-map)
;; "Alex"
```

Ajouter ou mettre à jour des entrées est simple :

```clojure
(def updated-map (assoc my-map :location "New York"))
;; {:name "Alex", :age 30, :location "New York"}

(def incremented-age (update my-map :age inc))
;; {:name "Alex", :age 31}
```

Pour supprimer des clés, utilisez `dissoc` :

```clojure
(def removed-age (dissoc my-map :age))
;; {:name "Alex"}
```

Pour itérer sur une map :

```clojure
(doseq [[k v] my-map] (println k "->" v))
;; :name -> Alex
;; :age -> 30
```

Et pour un accès conditionnel, `find` retourne une paire clé-valeur si la clé existe :

```clojure
(find my-map :age)
;; [:age 30]
```

## Approfondissement

Les tableaux associatifs dans Clojure, également communément appelés hash maps, sont incroyablement polyvalents et efficaces pour gérer des données basées sur des paires clé-valeur. Ils font partie de la riche bibliothèque de collections de Clojure, profondément enracinée dans la philosophie du langage de l'immutabilité et de la programmation fonctionnelle. Contrairement aux tableaux ou listes qui nécessitent une complexité temporelle de O(n) pour accéder aux éléments, les hash maps fournissent une complexité temporelle quasi constante pour l'accès, les rendant hautement efficaces pour les opérations de recherche.

On pourrait soutenir que les vecteurs dans Clojure pourraient servir un but similaire grâce à l'accès indexé, mais les hash maps se distinguent lorsqu'il s'agit de traiter des données non séquentielles et étiquetées, où la clé offre un descripteur significatif plutôt qu'un index arbitraire.

Unique à Clojure (et à son héritage Lisp), les tableaux associatifs sont des citoyens de première classe, ce qui signifie qu'ils peuvent être directement manipulés, passés autour des fonctions, et plus, sans avoir besoin d'une syntaxe spéciale ou de méthodes d'accès. Cette décision de conception renforce l'accent mis par Clojure sur la simplicité et la puissance.

Bien que les hash maps soient incroyablement utiles, il convient de mentionner que pour des ensembles de données très importants ou des scénarios où les clés sont hautement dynamiques (ajout et suppression constants), d'autres structures de données ou bases de données pourraient offrir de meilleures performances et flexibilité. Cependant, pour la plupart des cas d'utilisation typiques dans le domaine des applications Clojure, les tableaux associatifs fournissent un moyen robuste et efficace de gestion des données.
