---
title:                "Refactoring"
date:                  2024-01-26T01:17:38.434192-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/refactoring.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Le remaniement (Refactoring) est le processus de restructuration du code informatique existant sans en changer le comportement externe, visant à améliorer les attributs non fonctionnels. Les programmeurs remanient pour rendre leur code plus propre, plus efficace et plus facile à maintenir, améliorant ainsi efficacement la lisibilité et réduisant la complexité de leur logiciel.

## Comment faire :

Remanier dans Clojure—grâce à sa syntaxe épurée et son paradigme fonctionnel—peut être incroyablement simple. Abordons un scénario courant : l'itération sur des collections. Vous pourriez commencer avec une boucle `for`, comme ceci :

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

Appeler `(old-way)` nous donnera 55, la somme de 1 à 10. Mais, hé, nous pouvons remanier cela pour être plus Clojure-esque :

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

Cette fonction `(new-way)` remaniée utilise des macros de flux pour passer la plage directement à `reduce`, éliminant le surplus.

## Plongée profonde

L'art du remaniement trouve ses racines dans les premiers jours du développement logiciel mais a vraiment gagné du terrain avec le livre séminal de Martin Fowler "Refactoring: Improving the Design of Existing Code" publié en 1999. Dans Clojure, le remaniement s'appuie souvent sur les principes de programmation fonctionnelle, privilégiant les fonctions pures et les structures de données immuables.

Les alternatives au remaniement manuel dans Clojure pourraient inclure l'utilisation d'outils comme Cursive, un plugin populaire d'IntelliJ IDEA, qui offre des refontes automatisées spécifiques à Clojure. Il y a aussi clj-refactor, un paquet Emacs pour Clojure, fournissant une suite de fonctions de remaniement.

Un défi particulier au remaniement dans Clojure est de traiter avec l'état et les effets de bord dans un paradigme principalement immuable et sans effets de bord. L'utilisation prudente des atomes, refs, agents et transitoires sont essentiels pour maintenir à la fois la performance et la correction pendant les remaniements.

## Voir aussi

- "Refactoring: Improving the Design of Existing Code" de Martin Fowler pour les concepts fondamentaux.
- [Clojure Docs](https://clojuredocs.org/) pour des exemples spécifiques de code Clojure idiomatique.
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) pour l'automatisation du remaniement dans Emacs.
- [Cursive](https://cursive-ide.com/) pour les utilisateurs d'IntelliJ cherchant de l'aide automatisée pour le remaniement.
- [Refactoring with Rich Hickey](https://www.infoq.com/presentations/Simple-Made-Easy/) - Une conférence du créateur de Clojure qui, sans parler spécifiquement de remaniement, fournit un aperçu de la philosophie de Clojure pouvant guider des décisions de remaniement efficaces.